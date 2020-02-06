{-# LANGUAGE OverloadedStrings #-}

module Text.Nest.Tokens.Megaparsec.Broad
    ( wholeFile
    , string
    , stringEscapes
    , isSymbolChar
    ) where

import Text.Nest.Tokens

import Data.Functor ((<&>))
import Data.List (nub)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec, label, (<|>), many, choice)
import Text.Nest.Tokens.Megaparsec.Location (toLocation)

import qualified Data.Char as C
import qualified Data.Text as T
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Pos as P


type Parser = Parsec Void Text


wholeFile :: Parser [Token]
wholeFile = do
    tokens <- many anyToken
    last <- locatePayload endOfFile
    pure $ tokens ++ [last]

anyToken :: Parser Token
anyToken = locatePayload (choice payloads)
    where
    payloads =
        [ label "" whitespace
        , label "atom" word
        , label "comment" comment
        , label "heredoc" heredoc -- WARNING this must appear before `string`, b/c they both start with `"`
        , label "string" string
        , label "punctuation" (bracket <|> separator)
        ]

locatePayload :: Parser TokenPayload -> Parser Token
locatePayload action = do
    start <- P.getSourcePos
    payload <- action
    stop <- P.getSourcePos
    let loc = toLocation (start, stop)
    pure $ Token loc payload


------------ Whitespace ------------

whitespace :: Parser TokenPayload
whitespace = label "whitespace" (inline <|> newline)
    where
    inline = UnknownWhitespace . T.concat <$> P.some (simpleInline <|> splitline)
    simpleInline = P.takeWhile1P Nothing (`elem` (" \t" :: [Char]))
    splitline = P.string "\\\n"
    newline = UnknownNewline <$ P.string "\n"

-- TODO backslash-linebreak


-- NOTE: I'm thinking that inline comments are not so useful
-- Text editors don't deal well with nesting block comments, which is what I'd like if I wanted block comments
-- Also, a comment inside a li

comment :: Parser TokenPayload
comment = do
    _ <- P.char '#'
    text <- P.takeWhileP Nothing (/= '\n')
    pure Comment

endOfFile :: Parser TokenPayload
endOfFile = EndOfFile <$ P.eof


------------ Syntax ------------

word :: Parser TokenPayload
word = do
    -- for more options, peek around starting at https://www.compart.com/en/unicode/category
    UnknownAtom <$> P.takeWhile1P Nothing isSymbolChar

isSymbolChar :: Char -> Bool
isSymbolChar c = good c && defensive c
    where
    defensive c = c `notElem` ("\\# \t\n\r()[]{},.;:`\'\"" :: [Char])
    good c = C.isLetter c || C.isDigit c || nonModifyingSymbol c || c `elem` ("~!@$%^&*-_=+|<>/?" :: [Char])
    nonModifyingSymbol c = case C.generalCategory c of
        C.MathSymbol -> True
        C.CurrencySymbol -> True
        _ -> False

separator :: Parser TokenPayload
separator = do
    cs <- P.takeWhile1P Nothing (`elem` separatorChars)
    pure $ UnknownSeparator cs

bracket :: Parser TokenPayload
bracket = choice $ brackets <&> \(o, c) ->
    let open = Bracket Open o c <$ P.single o
        close = Bracket Close o c <$ P.single c
    in open <|> close


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

heredoc :: Parser TokenPayload
heredoc = do
    P.string "\"\"\""
    fence <- grabLine
    let parseFence = P.string ("\n" <> fence <> "\"\"\"") :: Parser Text
        loop soFar = P.try final <|> continue
            where
            final = soFar <$ parseFence
            continue = do
                P.single '\n'
                line <- grabLine
                loop (soFar <> "\n" <> line)
    lines <- loop ""
    pure $ String Plain lines Plain
    where
    grabLine = P.takeWhileP Nothing (/= '\n')

string :: Parser TokenPayload
string = do
    open <- strTemplJoin
    body <- T.concat <$> many stringSection
    close <- strTemplJoin
    pure $ String open body close

stringSection :: Parser Text
stringSection = plain <|> escape
    where
    plain = P.takeWhile1P Nothing (`notElem` ("\"`\\\r\n" :: [Char]))
    escape = do
        P.single '\\'
        c <- P.oneOf (fst <$> stringEscapes)
        case lookup c stringEscapes of
            Just p -> p
            Nothing -> error "internal error: an escape character was recognized without a corresponding escape parser"

strTemplJoin :: Parser StrTemplJoin
strTemplJoin = do
    c <- label "end of string" $ P.oneOf ("\"`" :: [Char])
    pure $ case c of { '\"' -> Plain ; '`' -> Templ }


stringEscapes :: [(Char, Parser Text)]
stringEscapes =
    [ ('\\', pure "\\")
    , ('0', pure "\0")
    , ('a', pure "\a")
    , ('b', pure "\b")
    , ('e', pure "\27")
    , ('f', pure "\f")
    , ('n', pure "\n")
    , ('r', pure "\r")
    , ('t', pure "\t")
    , ('v', pure "\v")
    , ('\'', pure "\'")
    , ('\"', pure "\"")
    -- WARNING: I am only allowing `\n` as a line separator; who wants to merge libraries with differing encodings for linesep?
    , ('\n', label "'\\' to resume string after linebreak" $ do
        _ <- P.takeWhileP Nothing (`elem` (" \t" :: [Char]))
        P.single '\\'
        pure ""
      )
    -- TODO \x[0-9a-fA-F]{2}
    -- TODO \u[0-9a-fA-F]{4}
    -- TODO \U(10|0[0-9a-fA-F])[0-9a-fA-F]{4}
    , ('&', pure "")
    ]
