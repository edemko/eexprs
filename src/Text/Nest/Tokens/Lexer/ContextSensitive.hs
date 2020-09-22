{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}

module Text.Nest.Tokens.Lexer.ContextSensitive
    ( contextualize
    ) where

import Text.Nest.Tokens.Types

import Control.Window.List (window2, window3)
import Text.Lightyear.Stream (advance)
import Text.Nest.Tokens.Lexer.Recognize (recognizeDepth)


contextualize :: [Lexeme (Result 'Free)] -> [Lexeme (Result 'Sens)]
contextualize xs =
       map packFree
    -- when thinking about indentation, I don't want to think about trailing whitespace/comments
    -- and any blank lines (modulo whitespace/comments)
    -- ignore these early so we only have one thing to worry about later (ignoring `IGN`, which shouldn't be hard)
    |> ignoreTrailing
    |> ignoreBlanklines
    -- these coordinate detecting and nexting indentation
    |> (recognizeColons |> detectIndentation |> ignoreStartIndents)
    -- WARNING detectIndentation must come before space-sensitive xformers, since leading spaces should not count as spaces
    -- so far, stuff only gets ignored at the start/end of a line, so we can just use windows for inline stuff
    -- (also, there are no `Space`s at the start of the line that aren't `IGN`d
    |> recognizeDots
    |> recognizeSpaces
    |> map ensureSensitive
    $ xs

ignoreTrailing :: [Lexeme SomeResult] -> [Lexeme SomeResult]
ignoreTrailing = go [] []
    where
    go buf acc [] = reverse (buf ++ acc)
    -- space and comment might get ignored
    go buf acc (x@L{payload=OK UnknownSpace} : xs) = go (x:buf) acc xs
    go buf acc (x@L{payload=OK Comment} : xs) = go (x:buf) acc xs
    -- and obviously already-ignored stuff is still ignored
    go buf acc (x@L{payload=IGN _} : xs) = go (x:buf) acc xs
    -- do ignore them when you find a newline immediately after
    go buf acc (x@L{payload=OK UnknownNewline} : xs) = go [] (x : fmap ignore buf ++ acc) xs
    -- don't ignore them if you find something other than a newline first
    go buf acc (x:xs) = go [] (x:buf ++ acc) xs

ignoreBlanklines :: [Lexeme SomeResult] -> [Lexeme SomeResult]
ignoreBlanklines = go (Just []) []
    where
    go Nothing acc [] = reverse acc
    go (Just buf) acc [] = reverse (fmap ignore buf ++ acc)
    -- a newline starts possible ignoring
    go Nothing acc (x@L{payload=OK UnknownNewline} : xs) = go (Just []) (x:acc) xs
    -- but nothing else switches to ignoring mode
    go Nothing acc (x : xs) = go Nothing (x:acc) xs
    -- adjacent newlines get ignored, and obvs also already-ignored stuff
    go (Just buf) acc (x@L{payload=OK UnknownNewline} : xs) = go (Just $ x:buf) acc xs
    go (Just buf) acc (x@L{payload=IGN _} : xs) = go (Just $ x:buf) acc xs
    -- once something other than a newline is found, go back to normal
    go (Just buf) acc (x : xs) = go Nothing (x : fmap ignore buf ++ acc) xs


data IndentState
    = StartOfFile
    | Search
    | FoundColon
    | FoundNewline [Lexeme SomeResult] (Lexeme SomeResult)
detectIndentation :: [Lexeme SomeResult] -> [Lexeme SomeResult]
detectIndentation = go StartOfFile [0] []
    where
    go ::
           IndentState
        -> [Int] -- stack of depths for each indent level, deepest on top
        -> [Lexeme SomeResult] -- accumulator, in reverse order
        -> [Lexeme SomeResult] -- remaining input stream
        -> [Lexeme SomeResult]
    -- I've written this so that it's easier to see the state machine
    go StartOfFile lvs acc = \case
        [] -> reverse acc
        x@L{payload=IGN _} : xs -> go StartOfFile lvs (x:acc) xs
        x@L{payload=OK UnknownSpace} : xs -> go Search lvs (x{payload=ERR IllegalIndent} : acc) xs
        x : xs -> go Search lvs (x : acc) xs
    go Search lvs acc = \case
        [] -> reverse acc
        x@L{payload=OK StartIndent} : xs -> go FoundColon lvs (x:acc) xs
        x@L{payload=OK UnknownNewline} : xs -> go (FoundNewline [] x) lvs acc xs
        x : xs -> go Search lvs (x:acc) xs
    go FoundColon lvs acc = \case
        x@L{payload=IGN _} : xs -> go FoundColon lvs (x:acc) xs
        x@L{payload=OK UnknownNewline} : xs -> go FoundColon lvs (ignore x:acc) xs
        x@L{loc,orig,payload=OK UnknownSpace} : xs -> case recognizeDepth loc orig of
            Left err -> go Search lvs (x{payload=ERR err}:acc) xs
            Right depth' -> if depth' > head lvs
                then go Search (depth':lvs) (x{payload=OK (Combiner Open Indent)}:acc) xs
                else go Search lvs (x{payload=ERR IllegalIndent}:acc) xs
        xs -> go Search lvs (err:acc) xs
            where
            err = case acc of
                [] -> error "detectIndentation: found colon but nothing is in the accumulator. please report"
                L{loc,orig} : _ -> L{loc=orig `advance` loc,orig="", payload=ERR IllegalIndent}
    go (FoundNewline buf nl) lvs acc = \case
        x@L{payload=IGN _} : xs -> go (FoundNewline (x:buf) nl) lvs acc xs
        x@L{payload=OK UnknownSpace} : xs ->
            offsides lvs x buf nl acc xs
        xs ->
            let zero = case acc of
                    [] -> error "detectIndentation: found newline but nothing is in the accumulator. please report"
                    L{loc,orig} : _ -> L{loc=orig `advance` loc,orig="", payload=OK UnknownSpace}
             in offsides lvs zero buf nl acc xs
    offsides ::
           [Int] -- indentation stack
        -> Lexeme SomeResult -- the spaces (incl. zero spaces)
        -> [Lexeme SomeResult] -- buffer after newline
        -> Lexeme SomeResult -- the newline
        -> [Lexeme SomeResult] -- accumulator
        -> [Lexeme SomeResult] -- the rest of the input stream
        -> [Lexeme SomeResult] -- recurse back into `go`
    offsides lvs0 x@L{loc,orig} buf nl acc xs = case recognizeDepth loc orig of
        Left err -> go Search lvs0 (x{payload=ERR err} : buf ++ ignore nl : acc) xs
        Right depth' -> drain lvs0 []
            where
            drain (lv:lvs) dedents
                | depth' < lv = drain lvs (dedent':dedents)
                | depth' == lv = if null dedents
                    then go Search (lv:lvs) (ignore x : buf ++ nl{payload=OK (Separator Newline)} : acc) xs
                    else go Search (lv:lvs) (nl' : ignore x : dedents ++ buf ++ ignore nl : acc) xs
                | depth' > lv = if null dedents
                    then go Search (lv:lvs) (ignore x : buf ++ ignore nl : acc) xs
                    else go Search (lv:lvs) (nl' : x{payload=ERR IllegalIndent} : dedents ++ buf ++ ignore nl : acc) xs
                | otherwise = error "wat"
            drain [] _ = error "detectIndentation: empty indentation stack. please report"
            dedent' = L{loc,orig="",payload=OK (Combiner Close Indent)}
            nl' = L{loc=orig `advance` loc,orig="",payload=OK (Separator Newline)}

ignoreStartIndents :: [Lexeme SomeResult] -> [Lexeme SomeResult]
ignoreStartIndents = map go
    where
    go x@L{payload=OK StartIndent} = ignore x
    go x = x

recognizeDots :: [Lexeme SomeResult] -> [Lexeme SomeResult]
recognizeDots = window3 go
    where
    -- I can get away with only checking for `Space` because newline and comments will only appear at beginning/end of line
    -- which don't count as a space anyway
    go :: Lexeme SomeResult -> Lexeme SomeResult -> Lexeme SomeResult -> Maybe [Lexeme SomeResult]
    go x@L{payload=OK UnknownSpace} y@L{payload=OK UnknownDot} z@L{payload=OK UnknownSpace}
        = Just [ignore x, y{payload=OK (Separator Dot)}, ignore z]
    -- except for illegal dots, where we can't ignore end-of-line stuff
    go x y@L{payload=OK UnknownDot} z@L{payload}
        | isSpacey payload = Just [x, y{payload=ERR IllegalDot}, z]
        where
        isSpacey (OK UnknownSpace) = True
        isSpacey (OK (Separator Newline)) = True
        isSpacey (OK UnknownNewline) = True
        isSpacey (IGN _) = True
        isSpacey _ = False
    go x@L{payload=OK UnknownSpace} y@L{payload=OK UnknownDot} z
        = Just [x, y{payload=OK SyntheticDot}, z]
    go x y@L{payload=OK UnknownDot} z
        = Just [x, y{payload=OK ChainDot}, z]
    go _ _ _ = Nothing

recognizeColons :: [Lexeme SomeResult] -> [Lexeme SomeResult]
recognizeColons = go Nothing []
    where
    go ::
           Maybe ([Lexeme SomeResult], Lexeme SomeResult) -- colon and buffer of following  tokens in reverse order
        -> [Lexeme SomeResult] -- accumulator in reverse order
        -> [Lexeme SomeResult] -- remaining input
        -> [Lexeme SomeResult]
    go Nothing acc [] = reverse acc
    -- end-of-file counts as newline for indentation purposes
    -- look for colons and switch mode when found
    go Nothing acc (x@L{payload=OK UnknownColon} : xs) =
        go (Just ([], x)) acc xs
    go Nothing acc (x : xs) = go Nothing (x : acc) xs
    -- if the first-non-ignored is a newline, then it's a start of indent
    go (Just (buf, colon)) acc (x@L{payload=IGN _} : xs) =
        go (Just (x:buf, colon)) acc xs
    go (Just (buf, colon)) acc (x@L{payload=OK UnknownNewline} : xs) =
        go Nothing (x : buf ++ colon{payload = OK StartIndent} : acc) xs
    go (Just (buf, colon)) acc [] = reverse $ buf ++ colon{payload=OK StartIndent} : acc
    -- otherwise, it's just a separator
    go (Just (buf, colon)) acc (x : xs) = go Nothing (buf ++ colon{payload=OK (Separator Colon)} : acc) (x : xs)

recognizeSpaces :: [Lexeme SomeResult] -> [Lexeme SomeResult]
recognizeSpaces = mkSeparatorSpaces . ignoreCombinerSpace . ignoreSeparatorSpace
    where
    ignoreSeparatorSpace = window2 go
        where
        go :: Lexeme SomeResult -> Lexeme SomeResult -> Maybe [Lexeme SomeResult]
        go x@L{payload=OK UnknownSpace} y@L{payload=OK (Separator _)} = Just [ignore x, y]
        go x@L{payload=OK (Separator _)} y@L{payload=OK UnknownSpace} = Just [x, ignore y]
        go _ _ = Nothing
    ignoreCombinerSpace = window2 go
        where
        go :: Lexeme SomeResult -> Lexeme SomeResult -> Maybe [Lexeme SomeResult]
        go x@L{payload=OK UnknownSpace} y@L{payload=OK (Combiner Close _)} = Just [ignore x, y]
        go x@L{payload=OK (Combiner Open _)} y@L{payload=OK UnknownSpace} = Just [x, ignore y]
        go x@L{payload=OK UnknownSpace} y@L{payload=OK (String Templ _ _)} = Just [ignore x, y]
        go x@L{payload=OK (String _ _ Templ)} y@L{payload=OK UnknownSpace} = Just [x, ignore y]
        go _ _ = Nothing
    mkSeparatorSpaces = map $ \case
        x@L{payload=OK UnknownSpace} -> x{payload=OK (Separator Space)}
        x -> x

packFree :: Lexeme (Result 'Free) -> Lexeme SomeResult
packFree x@L{payload} = x{payload=SO payload}

ensureSensitive :: Lexeme SomeResult -> Lexeme (Result 'Sens)
ensureSensitive tok@L{payload} = tok{payload=go payload}
    where
    go (OK (Atom x)) = Ok $ Atom x
    go (OK (String o str c)) = Ok $ String o str c
    go (OK (Combiner s c)) = Ok $ Combiner s $ case c of
        -- convert any phase to a Sens phase
        Paren -> Paren
        Brack -> Brack
        Brace -> Brace
        Indent -> Indent
    go (OK (Separator s)) = Ok $ Separator $ case s of
        -- convert any phase to a Sens phase
        Comma -> Comma
        Dot -> Dot
        Ellipsis -> Ellipsis
        Semicolon -> Semicolon
        Colon -> Colon
        Space -> Space
        Newline -> Newline
    go (OK ChainDot) = Ok ChainDot
    go (OK SyntheticDot) = Ok SyntheticDot
    go (OK StartIndent) = error "unexpected StartIndent after context-sensitive lexing. please report"
    go (OK Comment) = error "unexpected Comment after context-sensitive lexing. please report"
    go (OK UnknownSpace) = error "unexpected UnknownSpace after context-sensitive lexing. please report"
    go (OK UnknownNewline) = error "unexpected UnknownNewline after context-sensitive lexing. please report"
    go (OK UnknownDot) = error "unexpected UnknownDot after context-sensitive lexing. please report"
    go (OK UnknownColon) = error "unexpected UnknownColon after context-sensitive lexing. please report"
    go (IGN x) = Ignore x
    go (ERR err) = Error err


ignore :: Lexeme SomeResult -> Lexeme SomeResult
ignore r@L{payload} = r{payload=go payload}
    where
    go (OK x) = IGN x
    go x = x


infixl 9 |>
(|>) :: (a -> b) -> (b -> c) -> (a -> c)
(|>) = flip (.)

{-# COMPLETE OK, IGN, ERR #-}
pattern OK :: Token phase -> SomeResult
pattern OK x = SO (Ok x)
pattern IGN :: Token phase -> SomeResult
pattern IGN x = SO (Ignore x)
pattern ERR :: Error -> SomeResult
pattern ERR err = SO (Error err)
