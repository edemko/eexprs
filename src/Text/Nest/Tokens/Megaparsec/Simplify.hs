{-# LANGUAGE OverloadedStrings #-}

module Text.Nest.Tokens.Megaparsec.Simplify
    ( indent
    ) where

import Text.Nest.Tokens

import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec, (<|>), ParseErrorBundle)

import qualified Data.Text as T
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P


type Parser = Parsec Void Text

indent :: Parser TokenPayload
indent = do
    depth <- whitespace
    P.label "start of line" $ P.eof
    pure $ Indent depth

whitespace :: Parser Int
whitespace = sum <$> P.label "whitespace" (P.many (simpleInline <|> splitline))
    where
    simpleInline = T.length <$> P.takeWhile1P (Just "space") (==' ')
    splitline = 0 <$ P.label "line continuation" (P.string "\\\n")
