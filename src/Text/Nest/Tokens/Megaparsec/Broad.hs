{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Text.Nest.Tokens.Megaparsec.Broad
    ( wholeFile
    , stringEscapes
    , isSymbolChar
    ) where


import Text.Nest.Tokens.Types

import Data.Bifunctor (Bifunctor, bimap)
import Data.Functor ((<&>))
import Data.List (nub)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec, label, (<|>), many, choice)
import Text.Nest.Tokens.Megaparsec.Location (toLocation)
import Text.Nest.Tokens.Types.Broad (Result, Payload(..))

import qualified Data.Char as C
import qualified Data.Text as T
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Pos as P
import qualified Text.Nest.Tokens.Types.Broad as Broad


type Parser = Parsec Void Text


wholeFile :: Parser [LexResult Payload]
wholeFile = many anyToken

-- FIXME I'd like to return a `Broad.Result`, but that means reporting erros inside some parsers (e.g. heredoc)
anyToken :: Parser (LexResult Payload)
anyToken = choice
    [ whitespace
    , word
    , colonWord -- WARNING this must come before `separator` to extract words that start with a colon
    , comment
    , heredoc -- WARNING this must come before `string`, b/c they both start with `"`
    , string
    , bracket
    , separator
    -- , errToken -- TODO after returning `Result`
    ]


------------ Whitespace ------------

whitespace :: Parser (LexResult Payload)
whitespace = do
    pos0 <- P.getSourcePos
    (tok, orig) <- inline <|> newline
    pos' <- P.getSourcePos
    pure $ LR
        { loc = toLocation (pos0, pos')
        , orig
        , payload = tok
        }
    where
    inline = (Whitespace,) . T.concat <$> P.some (simpleInline <|> splitline)
    simpleInline = P.takeWhile1P Nothing (`elem` (" \t" :: [Char]))
    splitline = P.string "\\\n"
    newline = (Newline,) <$> P.string "\n"

-- TODO backslash-linebreak


-- NOTE: I'm thinking that inline comments are not so useful
-- Text editors don't deal well with nesting block comments, which is what I'd like if I wanted block comments
-- Also, a comment inside a li

comment :: Parser (LexResult Payload)
comment = do
    pos0 <- P.getSourcePos
    hash <- P.char '#'
    text <- P.takeWhileP Nothing (/= '\n')
    pos' <- P.getSourcePos
    let orig = hash `T.cons` text
    pure $ LR
        { loc = toLocation (pos0, pos')
        , orig
        , payload = Comment
        }


------------ Syntax ------------

word :: Parser (LexResult Payload)
word = do
    pos0 <- P.getSourcePos
    orig <- P.takeWhile1P Nothing isSymbolChar
    pos' <- P.getSourcePos
    pure LR
        { loc = toLocation (pos0, pos')
        , orig
        , payload = Atom
        }

colonWord :: Parser (LexResult Payload)
colonWord = P.try $ do
    pos0 <- P.getSourcePos
    colon <- P.char ':'
    rest <- P.takeWhile1P Nothing (\c -> isSymbolChar c || c == ':')
    pos' <- P.getSourcePos
    pure LR
        { loc = toLocation (pos0, pos')
        , orig = colon `T.cons` rest
        , payload = Atom
        }

-- for more options, peek around starting at https://www.compart.com/en/unicode/category
isSymbolChar :: Char -> Bool
isSymbolChar c = good c && defensive c
    where
    defensive c = c `notElem` ("\\# \t\n\r()[]{},.;:`\'\"" :: [Char])
    good c = C.isLetter c || C.isDigit c || nonModifyingSymbol c || c `elem` ("~!@$%^&*-_=+|<>/?" :: [Char])
    nonModifyingSymbol c = case C.generalCategory c of
        C.MathSymbol -> True
        C.CurrencySymbol -> True
        _ -> False

separator :: Parser (LexResult Payload)
separator = do
    pos0 <- P.getSourcePos
    orig <- P.takeWhile1P Nothing (`elem` separatorChars)
    pos' <- P.getSourcePos
    pure $ LR
        { loc = toLocation (pos0, pos')
        , orig
        , payload = Separator
        }

bracket :: Parser (LexResult Payload)
bracket = do
    pos0 <- P.getSourcePos
    (tok, c) <- choice $ brackets <&> \(o, c) ->
        let open = (Bracket Open o c,) <$> P.single o
            close = (Bracket Close o c,) <$> P.single c
        in open <|> close
    pos' <- P.getSourcePos
    pure LR
        { loc = toLocation (pos0, pos')
        , orig = T.singleton c
        , payload = tok
        }


separatorChars :: [Char]
separatorChars = ",.;:"

brackets :: [(Char, Char)]
brackets = map (\[o,c] -> (o, c)) db
    where
    db =
        [ "()"
        , "[]"
        , "{}"
        -- TODO a mix of fancier ones, or mixfixes
        -- I'm thinking that mixfixes handle one thing, but enclose and separate handles another.
        -- Thus, `operator none (_ + _) add` defines (i.e. `'operator' ('left'|'right'|'none') '(' '_'? (<name> '_')* <name>? ')' <name>`)
        -- wheras `comprehension ⟬ ⟭` would define a new parenthesis
        -- and `comprehension ⟬ , ⟭` would define one which has elements separated by commas
        -- I'm tempted to have even comprehension ⟬ , ; ⟭` which would have semicolon-separated lists of comma-separated elements
        ]


------------ Strings ------------

heredoc :: Parser (LexResult Payload)
heredoc = do
    pos0 <- P.getSourcePos
    -- parse the open mark
    quotes <- P.string "\"\"\""
    fence <- grabToLine
    nl <- T.singleton <$> P.char '\n'
    let open = quotes <> fence <> nl
    -- parse the body
    let startLoop = P.try (endLoop "") <|> (grabToLine >>= loop)
        loop soFar = P.try (endLoop soFar) <|> continue soFar
        continue soFar = do
            c <- P.char '\n'
            line <- grabToLine
            loop (soFar <> T.cons c line)
    -- parse the close mark
        endLoop soFar = (soFar,) <$> parseFence
        parseFence = P.string ("\n" <> fence <> "\"\"\"") :: Parser Text
    (lines, close) <- startLoop
    pos' <- P.getSourcePos
    pure LR
        { loc = toLocation (pos0, pos')
        , orig = open <> lines <> close
        , payload = String Plain lines Plain
        }
    where
    grabToLine :: Parser Text
    grabToLine = P.takeWhileP Nothing (/= '\n')

string :: Parser (LexResult Payload)
string = do
    pos0 <- P.getSourcePos
    (openChar, open) <- strTemplJoin
    (orig, body) <- biconcat <$> many stringSection
    (closeChar, close) <- strTemplJoin
    pos' <- P.getSourcePos
    pure LR
        { loc = toLocation (pos0, pos')
        , orig = openChar <> orig <> closeChar
        , payload = String open body close
        }

stringSection :: Parser (Text, Text)
stringSection = plain <|> escape
    where
    plain = dup <$> P.takeWhile1P Nothing (`notElem` ("\"`\\\r\n" :: [Char]))
    escape = do
        bs <- P.single '\\'
        c <- P.oneOf (fst <$> stringEscapes)
        case lookup c stringEscapes of
            Nothing -> error "internal error: an escape character was recognized without a corresponding escape parser"
            Just p -> do
                (orig, sem) <- p
                pure (bs `T.cons` c `T.cons` orig, sem)

strTemplJoin :: Parser (Text, StrTemplJoin)
strTemplJoin = do
    c <- P.oneOf ("\"`" :: [Char])
    let semantic = case c of { '\"' -> Plain ; '`' -> Templ }
    pure (T.singleton c, semantic)


stringEscapes :: [(Char, Parser (Text, Text))]
stringEscapes = (fromBasic <$> basicEscapes) ++ fancyEscapes
    where
    fromBasic :: (Char, Char) -> (Char, Parser (Text, Text))
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
        [ ('\n', label "'\\' to resume string after linebreak" $ do
            leading <- P.takeWhileP Nothing (`elem` (" \t" :: [Char]))
            resume <- P.single '\\'
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
