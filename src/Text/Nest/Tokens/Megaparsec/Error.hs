{-# LANGUAGE NamedFieldPuns #-}
module Text.Nest.Tokens.Megaparsec.Error where

import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (ParseErrorBundle(..), SourcePos(..))
import Text.Nest.Tokens.Types (Location(..),LexError(..))

import qualified Text.Megaparsec as P


toLexError :: Location -> ParseErrorBundle Text Void -> LexError
toLexError loc0 ParseErrorBundle{bundleErrors,bundlePosState} = case bundleErrors of
    -- x :| xs@(_:_) ->
    P.TrivialError off unexpected expected :| [] ->
        let (_, st) = P.reachOffset off bundlePosState
            SourcePos{sourceLine,sourceColumn} = P.pstateSourcePos st
            errLoc = loc0{fromLine = P.unPos sourceLine, fromCol = P.unPos sourceColumn}
            --
            reason = maybe "TODO" (("unexpected "++) . fromErrorItem) unexpected
         in LexError{errLoc, reason, suggestions=[]}
    -- _ -> error $ unlines [show errs, P.errorBundlePretty errs] -- FIXME
    where
    fromErrorItem :: P.ErrorItem Char -> String
    fromErrorItem (P.Tokens xs) = show (toList xs)
    fromErrorItem (P.Label xs) = toList xs
    fromErrorItem P.EndOfInput = "end of input"