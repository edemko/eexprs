{-# LANGUAGE NamedFieldPuns #-}
module Text.EExpr.Tokens.Lexer.Error
    ( Error(..)
    , expect
    , crammedTokens
    , mixedIndent
    , panic
    ) where

import Text.Lightyear ()
import Text.Lightyear.Error

import Data.Text (Text)
import Text.EExpr.Tokens.Types (Error(..))

import qualified Data.Text as T


expect :: [String] -> MakeError st Text Error
expect expected st =
    let found = fst <$> T.uncons (input st)
     in Unexpected (position st) found expected

crammedTokens :: MakeError st Text Error
crammedTokens st = CrammedTokens (position st) (input st)

mixedIndent :: MakeError st Text Error
mixedIndent st =
    let Just found = fst <$> T.uncons (input st)
     in MixedIndent (position st) found

panic :: String -> MakeError st strm Error
panic explain = const $ Panic explain
