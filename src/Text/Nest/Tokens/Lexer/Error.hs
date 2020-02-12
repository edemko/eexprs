{-# LANGUAGE NamedFieldPuns #-}
module Text.Nest.Tokens.Lexer.Error
    ( LexError(..)
    , expect
    , crammedTokens
    , mixedIndent
    , panic
    ) where

import Text.Lightyear ()
import Text.Lightyear.Error

import Data.Text (Text)
import Text.Nest.Tokens.Types (LexError(..))

import qualified Data.Text as T


expect :: [String] -> MakeError st Text LexError
expect expected st =
    let found = fst <$> T.uncons (input st)
     in Unexpected (position st) found expected

crammedTokens :: MakeError st Text LexError
crammedTokens st = CrammedTokens (position st) (input st)

mixedIndent :: MakeError st Text LexError
mixedIndent st =
    let Just found = fst <$> T.uncons (input st)
     in MixedIndent (position st) found

panic :: String -> MakeError st strm LexError
panic explain = const $ Panic explain
