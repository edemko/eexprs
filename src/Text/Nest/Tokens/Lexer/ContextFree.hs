{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Text.Nest.Tokens.Lexer.ContextFree
    ( parse
    , stringEscapes
    , isSymbolChar
    ) where

import Prelude hiding (lines)
import Text.Nest.Tokens.Types

import Data.Functor ((<&>))
import Data.Text (Text)
import Text.Lightyear (Lightyear, Consume(..), Branch(..))
import Text.Nest.Tokens.Lexer.Error (expect, panic)
import Text.Nest.Tokens.Lexer.Recognize (isSymbolChar, recognizeAtom, recognizeSeparator)

import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Text.Lightyear as P


parse :: Text -> [Result]
parse inp = case P.runLightyear wholeFile inp () of
    Right toks -> toks
    Left err -> error $ "Internal Nest Error! Please report.\nLexer failed to recover: " ++ show err


type Parser c a = Lightyear c () Text LexError a


wholeFile :: Parser 'Greedy [Result]
wholeFile = do
    toks <- many anyToken
    P.endOfInput (panic "broad lexer failed to reach end of input")
    pure toks

anyToken :: Parser 'Greedy Result
anyToken = P.choice $ NE.fromList
    [ fmap Ok <$> whitespace
    , word
    , P.fromAtomic colonWord -- WARNING this must come before `separator` to extract words that start with a colon
    , fmap Ok <$> comment
    , heredoc -- WARNING this must come before `string`, b/c they both start with `"`
    , string
    , fmap Ok <$> bracket
    , separator
    , errToken
    ]

errToken :: Parser 'Greedy Result
errToken = do
    loc <- P.getPosition
    c <- P.any (panic "errToken")
    pure LR
        { loc
        , orig = T.singleton c
        , payload = Error $ BadChar loc c
        }

------------ Whitespace ------------

whitespace :: Parser 'Greedy (LexResult Payload)
whitespace = do
    pos0 <- P.getPosition
    (tok, orig) <- inline <|> newline
    pure $ LR
        { loc = pos0
        , orig
        , payload = tok
        }
    where
    inline = (Space,) . T.concat <$> P.some (simpleInline <|> splitline)
    simpleInline = P.takeWhile1 (panic "simple whitespace") (`elem` [' ', '\t'])
    splitline = P.string (panic "split whitespace") "\\\n"
    newline = (UnknownNewline,) <$> P.string (panic "newline") "\n"

-- TODO backslash-linebreak


-- NOTE: I'm thinking that block comments are not so useful
-- Text editors don't deal well with nesting block comments, which is what I'd like if I wanted block comments

comment :: Parser 'Greedy (LexResult Payload)
comment = do
    pos0 <- P.getPosition
    hash <- P.char (panic "comment") '#'
    text <- P.takeWhile (/= '\n')
    let orig = hash `T.cons` text
    pure $ LR
        { loc = pos0
        , orig
        , payload = Comment
        }


------------ Syntax ------------

word :: Parser 'Greedy Result
word = do
    pos0 <- P.getPosition
    orig <- P.takeWhile1 (panic "word") isSymbolChar
    pure LR
        { loc = pos0
        , orig
        , payload = recognizeAtom pos0 orig
        }

colonWord :: Parser 'Atomic Result
colonWord = P.try $ do
    pos0 <- P.getPosition
    colon <- P.char (panic "colon-word") ':'
    rest <- P.takeWhile1 (panic "colon-word") (\c -> isSymbolChar c || c == ':')
    let orig = colon `T.cons` rest
    pure LR
        { loc = pos0
        , orig
        , payload = recognizeAtom pos0 orig
        }

separator :: Parser 'Greedy Result
separator = do
    pos0 <- P.getPosition
    orig <- P.takeWhile1 (panic "separator consumed") (`elem` separatorChars)
    pure $ LR
        { loc = pos0
        , orig
        , payload = recognizeSeparator pos0 orig
        }

bracket :: Parser 'Greedy (LexResult Payload)
bracket = do
    pos0 <- P.getPosition
    (tok, c) <- P.choice $ NE.fromList $ combiners <&> \(sem, o, c) ->
        let open = (Combiner Open sem,) <$> P.char (panic "open bracket") o
            close = (Combiner Close sem,) <$> P.char (panic "close bracket") c
        in open <|> close
    pure LR
        { loc = pos0
        , orig = T.singleton c
        , payload = tok
        }


separatorChars :: [Char]
separatorChars = ",.;:"

combiners :: [(Combiner, Char, Char)]
combiners = map (\(sem, [o,c]) -> (sem, o, c)) db
    where
    db =
        [ (Paren, "()")
        , (Brack, "[]")
        , (Brace, "{}")
        -- TODO a mix of fancier ones, or mixfixes
        -- I'm thinking that mixfixes handle one thing, but enclose and separate handles another.
        -- Thus, `operator none (_ + _) add` defines (i.e. `'operator' ('left'|'right'|'none') '(' '_'? (<name> '_')* <name>? ')' <name>`)
        -- wheras `comprehension ⟬ ⟭` would define a new parenthesis
        -- and `comprehension ⟬ , ⟭` would define one which has elements separated by commas
        -- I'm tempted to have even comprehension ⟬ , ; ⟭` which would have semicolon-separated lists of comma-separated elements
        ]


------------ Strings ------------

heredoc :: Parser 'Greedy Result
heredoc = do
    pos0 <- P.getPosition
    (origOpen, fence) <- openHeredoc
    lines <- T.concat <$> P.many1 (firstLine fence) (bodyLines fence)
    closeE <- P.recover (P.fromAtomic $ endHeredoc fence)
    pure $ case closeE of
        Right origClose -> LR
            { loc = pos0
            , orig = origOpen <> lines <> origClose
            , payload = Ok $ String Plain lines Plain
            }
        Left err -> LR
            { loc = pos0
            , orig = origOpen <> lines
            , payload = Error err
            }
    where
    grabToLine :: Parser c Text
    grabToLine = P.takeWhile (/= '\n')
    openHeredoc :: Parser 'Greedy (Text, Text)
    openHeredoc = do
        quotes <- P.string (panic "start of heredoc") "\"\"\""
        fence <- grabToLine -- FIXME should take only alphanum (and be non-empty?)
        nl <- T.singleton <$> P.char (expect ["newline"]) '\n'
        pure (quotes <> fence <> nl, fence)
    firstLine :: Text -> Parser 'Greedy Text
    firstLine fence = do
        P.notFollowedBy (const $ Panic "heredoc fence (first line)") (endHeredocFirst fence)
        grabToLine
    bodyLines :: Text -> Parser 'Greedy Text
    bodyLines fence = do
        P.notFollowedBy (const $ Panic "heredoc fence") (endHeredoc fence)
        c <- P.char (expect ["end of heredoc"]) '\n'
        line <- grabToLine
        pure (T.cons c line)
    endHeredoc :: Text -> Parser 'Atomic Text
    endHeredoc fence = P.unsafeToAtomic $ P.string (expect ["end of heredoc"]) ("\n" <> fence <> "\"\"\"")
    endHeredocFirst :: Text -> Parser 'Atomic Text
    endHeredocFirst fence = P.unsafeToAtomic $ P.string (expect ["end of heredoc"]) (fence <> "\"\"\"")

string :: Parser 'Greedy Result
string = do
    pos0 <- P.getPosition
    (openChar, open) <- strTemplJoin
    (orig, body) <- biconcat <$> many stringSection
    closeE <- P.recover strTemplJoin
    pure $ case closeE of
        Right (closeChar, close) -> LR
            { loc = pos0
            , orig = openChar <> orig <> closeChar
            , payload = Ok $ String open body close
            }
        Left err -> LR
            { loc = pos0
            , orig = openChar <> orig
            , payload = Error err
            }

stringSection :: Parser 'Greedy (Text, Text)
stringSection = plain <|> escape
    where
    plain = dup <$> P.takeWhile1 (panic "plain string characters") (`notElem` ['\"', '`', '\\', '\r', '\n'])
    escape = do
        bs <- P.char (panic "string escape") '\\'
        let nextChars = fst <$> stringEscapes
        c <- P.satisfy (expect $ (:[]) <$> nextChars) (`elem` nextChars)
        case lookup c stringEscapes of
            Nothing -> error "internal error: an escape character was recognized without a corresponding escape parser"
            Just p -> do
                (orig, sem) <- p
                pure (bs `T.cons` c `T.cons` orig, sem)

strTemplJoin :: Parser 'Greedy (Text, StrTemplJoin)
strTemplJoin = do
    c <- P.satisfy (expect ["end of string", "start of splice"]) (`elem` ['\"', '`'])
    let semantic = case c of { '\"' -> Plain ; '`' -> Templ; _ -> error "Internal Nest Error" }
    pure (T.singleton c, semantic)


stringEscapes :: [(Char, Parser 'Greedy (Text, Text))]
stringEscapes = (fromBasic <$> basicEscapes) ++ fancyEscapes
    where
    -- Produces the character expected, as well as a parser
    -- which is run after the character has matched.
    -- That parser produces the original after the character
    -- and the whole escape's semantics.
    fromBasic :: (Char, Char) -> (Char, Parser 'Greedy (Text, Text))
    fromBasic (c, sem) = (c, pure $ ("", T.singleton sem))
    basicEscapes :: [(Char, Char)]
    basicEscapes =
        [ ('\\', '\\')
        , ('0', '\0')
        , ('a', '\a')
        , ('b', '\b')
        , ('e', '\27')
        , ('f', '\f')
        , ('n', '\n')
        , ('r', '\r')
        , ('t', '\t')
        , ('v', '\v')
        , ('\'', '\'')
        , ('\"', '\"')
        ]
    fancyEscapes =
        -- TODO \x[0-9a-fA-F]{2}
        -- TODO \u[0-9a-fA-F]{4}
        -- TODO \U(10|0[0-9a-fA-F])[0-9a-fA-F]{4}
        -- WARNING: I am only allowing `\n` as a line separator; who wants to merge libraries with differing encodings for linesep?
        [ ('\n', do
            leading <- P.takeWhile (`elem` [' ', '\t'])
            resume <- P.char (expect ["'\\' to resume string after linebreak"]) '\\'
            pure (leading `T.snoc` resume , "\n")
          )
        , ('&', pure ("", ""))
        ]

dup :: a -> (a, a)
dup x = (x, x)

biconcat :: (Monoid a, Monoid b) => [(a, b)] -> (a, b)
biconcat [] = (mempty, mempty)
biconcat ((a, b) : rest) = (a <> restA, b <> restB)
    where (restA, restB) = biconcat rest
