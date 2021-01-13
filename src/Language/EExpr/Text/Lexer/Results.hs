{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternSynonyms #-}

module Language.EExpr.Text.Lexer.Results
  ( Result(..)
  , Error(..)
  , SomeResult(OK, IGN, ERR)
  , expect
  , mixedIndent
  , panic
  ) where

import Data.Kind (Type)
import Data.Text (Text)
import Language.EExpr.Text.Lexer.Types (Phase(..),Token(..))
import Text.Lightyear (TextPos(..),input,position)
import Text.Lightyear.Error (MakeError)

import qualified Data.Text as T



-- | Existentially quantifies over the 'Phase' index of 'Result'.
-- We need this so that we can work on a heterogenously phase-indexed 'Token' stream during context-sensitive parsing.
data SomeResult = forall phase. SO (Result phase)

{-# COMPLETE OK, IGN, ERR #-}
pattern OK :: Token phase -> SomeResult
pattern OK x = SO (Ok x)
pattern IGN :: Token phase -> SomeResult
pattern IGN x = SO (Ignore x)
pattern ERR :: Error -> SomeResult
pattern ERR err = SO (Error err)

data Result :: Phase -> Type where
    Ok :: Token phase -> Result phase
    Ignore :: Token phase -> Result 'Clean
    Error :: Error -> Result phase

data Error
    = BadChar TextPos Char
    | Unexpected TextPos (Maybe Char) [String] -- unexpected character/end-of-input, expected set
    | CrammedTokens TextPos Text -- tokens after first
    | MixedIndent TextPos Char -- unexpected whitespace char
    | IllegalDot -- dot with whitespace after, but not before
    | IllegalIndent -- if file starts with indent, or indent is smaller than previous level
    | Panic String -- for when error conditions shouldn't get thrown
    | Bundle [Error]
    deriving (Show)

instance Semigroup Error where
    (Panic _) <> b = b
    a <> (Panic _) = a
    (Bundle as) <> (Bundle bs) = Bundle (as <> bs)
    (Bundle as) <> b = Bundle (as ++ [b])
    a <> (Bundle bs) = Bundle (a : bs)
    a <> b = Bundle [a, b]

instance Show (Result phase) where
    showsPrec p tok = showParen (p >= 11) $
        case tok of
            Ok x -> showString "Ok " . showsPrec 11 x
            Ignore x -> showString "Ignore " . showsPrec 11 x
            Error err -> showString "Error " . showsPrec 11 err


------------ Parser Helpers ------------

expect :: [String] -> MakeError st Text Error
expect expected st =
    let found = fst <$> T.uncons (input st)
     in Unexpected (position st) found expected

mixedIndent :: MakeError st Text Error
mixedIndent st =
    let Just found = fst <$> T.uncons (input st)
     in MixedIndent (position st) found

panic :: String -> MakeError st strm Error
panic explain = const $ Panic explain
