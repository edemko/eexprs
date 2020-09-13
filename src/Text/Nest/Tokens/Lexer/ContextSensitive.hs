{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Text.Nest.Tokens.Lexer.ContextSensitive
    ( contextualize
    ) where

import Text.Nest.Tokens.Types

import Control.Window.List (window2, window3)
import Text.Lightyear.Stream (advance)
import Text.Nest.Tokens.Lexer.Recognize (recognizeDepth)


contextualize :: [Result] -> [Result]
contextualize xs =
    -- when thinking about indentation, I don't want to think about trailing whitespace/comments
    -- and any blank lines (modulo whitespace/comments)
    -- ignore these early so we only have one thing to worry about later (ignoring `Ignore`, which shouldn't be hard)
       ignoreTrailing
    |> ignoreBlanklines
    -- these coordinate detecting and nexting indentation
    |> (recognizeColons |> detectIndentation |> ignoreStartIndents)
    -- WARNING detectIndentation must come before space-sensitive xformers, since leading spaces should not count as spaces
    -- so far, stuff only gets ignored at the start/end of a line, so we can just use windows for inline stuff
    -- (also, there are no `Space`s at the start of the line that aren't `Ignore`d
    |> recognizeDots
    |> ignoreCombinerSpace
    |> ignoreSeparatorSpace
    $ xs

ignoreTrailing :: [Result] -> [Result]
ignoreTrailing = go [] []
    where
    go buf acc [] = reverse (buf ++ acc)
    -- space and comment might get ignored
    go buf acc (x@LR{payload=Ok Space} : xs) = go (x:buf) acc xs
    go buf acc (x@LR{payload=Ok Comment} : xs) = go (x:buf) acc xs
    -- and obviously already-ignored stuff is still ignored
    go buf acc (x@LR{payload=Ignore _} : xs) = go (x:buf) acc xs
    -- do ignore them when you find a newline immediately after
    go buf acc (x@LR{payload=Ok UnknownNewline} : xs) = go [] (x : fmap ignore buf ++ acc) xs
    -- don't ignore them if you find something other than a newline first
    go buf acc (x:xs) = go [] (x:buf ++ acc) xs

ignoreBlanklines :: [Result] -> [Result]
ignoreBlanklines = go (Just []) []
    where
    go Nothing acc [] = reverse acc
    go (Just buf) acc [] = reverse (fmap ignore buf ++ acc)
    -- a newline starts possible ignoring
    go Nothing acc (x@LR{payload=Ok UnknownNewline} : xs) = go (Just []) (x:acc) xs
    -- but nothing else switches to ignoring mode
    go Nothing acc (x : xs) = go Nothing (x:acc) xs
    -- adjacent newlines get ignored, and obvs also already-ignored stuff
    go (Just buf) acc (x@LR{payload=Ok UnknownNewline} : xs) = go (Just $ x:buf) acc xs
    go (Just buf) acc (x@LR{payload=Ignore _} : xs) = go (Just $ x:buf) acc xs
    -- once something other than a newline is found, go back to normal
    go (Just buf) acc (x : xs) = go Nothing (x : fmap ignore buf ++ acc) xs


data IndentState
    = StartOfFile
    | Search
    | FoundColon
    | FoundNewline [Result] Result
detectIndentation :: [Result] -> [Result]
detectIndentation = go StartOfFile [0] []
    where
    go ::
           IndentState
        -> [Int] -- stack of depths for each indent level, deepest on top
        -> [Result] -- accumulator, in reverse order
        -> [Result] -- remaining input stream
        -> [Result]
    -- I've written this so that it's easier to see the state machine
    go StartOfFile lvs acc = \case
        [] -> reverse acc
        x@LR{payload=Ignore _} : xs -> go StartOfFile lvs (x:acc) xs
        x@LR{payload=Ok Space} : xs -> go Search lvs (x{payload=Error IllegalIndent} : acc) xs
        x : xs -> go Search lvs (x : acc) xs
    go Search lvs acc = \case
        [] -> reverse acc
        x@LR{payload=Ok StartIndent} : xs -> go FoundColon lvs (x:acc) xs
        x@LR{payload=Ok UnknownNewline} : xs -> go (FoundNewline [] x) lvs acc xs
        x : xs -> go Search lvs (x:acc) xs
    go FoundColon lvs acc = \case
        x@LR{payload=Ignore _} : xs -> go FoundColon lvs (x:acc) xs
        x@LR{payload=Ok UnknownNewline} : xs -> go FoundColon lvs (ignore x:acc) xs
        x@LR{loc,orig,payload=Ok Space} : xs -> case recognizeDepth loc orig of
            Left err -> go Search lvs (x{payload=Error err}:acc) xs
            Right depth' -> if depth' > head lvs
                then go Search (depth':lvs) (x{payload=Ok (Combiner Open Indent)}:acc) xs
                else go Search lvs (x{payload=Error IllegalIndent}:acc) xs
        xs -> go Search lvs (err:acc) xs
            where
            err = case acc of
                [] -> error "detectIndentation: found colon but nothing is in the accumulator. please report"
                LR{loc,orig} : _ -> LR{loc=orig `advance` loc,orig="", payload=Error IllegalIndent}
    go (FoundNewline buf nl) lvs acc = \case
        x@LR{payload=Ignore _} : xs -> go (FoundNewline (x:buf) nl) lvs acc xs
        x@LR{payload=Ok Space} : xs ->
            offsides lvs x buf nl acc xs
        xs ->
            let zero = case acc of
                    [] -> error "detectIndentation: found newline but nothing is in the accumulator. please report"
                    LR{loc,orig} : _ -> LR{loc=orig `advance` loc,orig="", payload=Ok Space}
             in offsides lvs zero buf nl acc xs
    offsides ::
           [Int] -- indentation stack
        -> Result -- the spaces (incl. zero spaces)
        -> [Result] -- buffer after newline
        -> Result -- the newline
        -> [Result] -- accumulator
        -> [Result] -- the rest of the input stream
        -> [Result] -- recurse back into `go`
    offsides lvs0 x@LR{loc,orig} buf nl acc xs = case recognizeDepth loc orig of
        Left err -> go Search lvs0 (x{payload=Error err} : buf ++ ignore nl : acc) xs
        Right depth' -> drain lvs0 []
            where
            drain (lv:lvs) dedents
                | depth' < lv = drain lvs (dedent':dedents)
                | depth' == lv = if null dedents
                    then go Search (lv:lvs) (ignore x : buf ++ nl{payload=Ok (Separator Newline)} : acc) xs
                    else go Search (lv:lvs) (nl' : ignore x : dedents ++ buf ++ ignore nl : acc) xs
                | depth' > lv = go Search (lv:lvs) (nl' : x{payload=Error IllegalIndent} : dedents ++ buf ++ ignore nl : acc) xs
                | otherwise = error "wat"
            drain [] _ = error "detectIndentation: empty indentation stack. please report"
            dedent' = LR{loc,orig="",payload=Ok (Combiner Close Indent)}
            nl' = LR{loc=orig `advance` loc,orig="",payload=Ok (Separator Newline)}

ignoreStartIndents :: [Result] -> [Result]
ignoreStartIndents = map go
    where
    go x@LR{payload=Ok StartIndent} = ignore x
    go x = x

recognizeDots :: [Result] -> [Result]
recognizeDots = window3 go
    where
    -- I can get away with only checking for `Space` because newline and comments will only appear at beginning/end of line
    -- which don't count as a space anyway
    go x@LR{payload=Ok Space} y@LR{payload=Ok SensitiveDot} z@LR{payload=Ok Space}
        = Just [ignore x, y{payload=Ok (Separator Dot)}, ignore z]
    -- except for illegal dots, where we can't ignore end-of-line stuff
    go x y@LR{payload=Ok SensitiveDot} z@LR{payload}
        | isSpacey payload = Just [x, y{payload=Error IllegalDot}, z]
        where
        isSpacey (Ok Space) = True
        isSpacey (Ok (Separator Newline)) = True
        isSpacey (Ok UnknownNewline) = True
        isSpacey (Ignore _) = True
        isSpacey _ = False
    go x@LR{payload=Ok Space} y@LR{payload=Ok SensitiveDot} z
        = Just [x, y{payload=Ok SyntheticDot}, z]
    go x y@LR{payload=Ok SensitiveDot} z
        = Just [x, y{payload=Ok ChainDot}, z]
    go _ _ _ = Nothing

recognizeColons :: [Result] -> [Result]
recognizeColons = go Nothing []
    where
    go Nothing acc [] = reverse acc
    -- end-of-file counts as newline for indentation purposes
    -- look for colons and switch mode when found
    go Nothing acc (x@LR{payload=Ok (SensitiveColon)} : xs) =
        go (Just ([], x)) acc xs
    go Nothing acc (x : xs) = go Nothing (x : acc) xs
    -- if the first-non-ignored is a newline, then it's a start of indent
    go (Just (buf, colon)) acc (x@LR{payload=Ignore _} : xs) =
        go (Just (x:buf, colon)) acc xs
    go (Just (buf, colon)) acc (x@LR{payload=Ok UnknownNewline} : xs) =
        go Nothing (x : buf ++ colon{payload = Ok StartIndent} : acc) xs
    go (Just (buf, colon)) acc [] = reverse $ buf ++ colon{payload=Ok StartIndent} : acc
    -- otherwise, it's just a separator
    go (Just (buf, colon)) acc (x : xs) = go Nothing (buf ++ colon{payload=Ok (Separator Colon)} : acc) (x : xs)

ignoreSeparatorSpace :: [Result] -> [Result]
ignoreSeparatorSpace = window2 go
    where
    go x@LR{payload=Ok Space} y@LR{payload=Ok (Separator _)} = Just [ignore x, y]
    go x@LR{payload=Ok (Separator _)} y@LR{payload=Ok Space} = Just [x, ignore y]
    go _ _ = Nothing

ignoreCombinerSpace :: [Result] -> [Result]
ignoreCombinerSpace = window2 go
    where
    go x@LR{payload=Ok Space} y@LR{payload=Ok (Combiner Close _)} = Just [ignore x, y]
    go x@LR{payload=Ok (Combiner Open _)} y@LR{payload=Ok Space} = Just [x, ignore y]
    go x@LR{payload=Ok Space} y@LR{payload=Ok (String Templ _ _)} = Just [ignore x, y]
    go x@LR{payload=Ok (String _ _ Templ)} y@LR{payload=Ok Space} = Just [x, ignore y]
    go _ _ = Nothing

ignore :: Result -> Result
ignore r@LR{payload} = r{payload=go payload}
    where
    go (Ok x) = Ignore x
    go x = x


infixl 9 |>
(|>) :: (a -> b) -> (b -> c) -> (a -> c)
(|>) = flip (.)
