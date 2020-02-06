{-# LANGUAGE OverloadedStrings #-}

module Text.Nest.Tokens.Megaparsec.Recognize
    ( recognizeAtom
    , recognizeSeparator
    ) where

import Text.Nest.Tokens

import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec, (<|>), SourcePos, ParseErrorBundle)
import Text.Nest.Tokens.Megaparsec.Broad (isSymbolChar)

import qualified Data.Char as C
import qualified Data.Text as T
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Pos as P


recognizeAtom :: Parser Atom
recognizeAtom = do
    atom <- parseAtom
    P.label "end of token" P.eof
    pure atom

recognizeSeparator :: Parser Text
recognizeSeparator = do
    sep <- parseSeparator
    P.label "end of token" P.eof
    pure sep


type Parser = Parsec Void Text

parseAtom :: Parser Atom
parseAtom = do
    parser <- (parseNum <$ isNum) <|> (parseSymbol <$ isSymbol)
    parser
    where
    isNum = P.lookAhead . P.try $ do
        _ <- P.optional (P.oneOf ['+', '-'])
        P.satisfy C.isDigit
    isSymbol = pure ()
    parseNum = do
        sign <- P.option 1 $ (1 <$ P.char '+') <|> ((-1) <$ P.char '-')
        -- FIXME I've only impelmented decimal integers
        whole <- read . T.unpack <$> P.takeWhile1P (Just "digit") C.isDigit
        pure $ IntAtom (sign * whole)
    parseSymbol = SymAtom <$> P.takeWhile1P (Just "symbol character") isSymbolChar -- WARNING is too permissive, but it doesn't matter b/c isNum is checked before isSymbol

parseSeparator :: Parser Text
parseSeparator =
    P.choice [ P.label l $
        P.choice (P.string <$> ss)
    | (l, ss) <- separators
    ]


separators :: [(String, [Text])]
separators =
    -- WARNING: each token must come after all tokens it prefixes
    [ ("comma", [","])
    , ("double-dot", [".."])
    , ("dot", ["."])
    , ("semicolon", [";"])
    -- , ("", [";;"])
    , ("colon", [":"])
    -- , ("", ["::"]) -- TODO or perhaps, colons could be part of identifiers, and just a single colon is punctuation
    ]
