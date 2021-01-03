{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternSynonyms #-}

module Text.EExpr.Tokens.Parser.Types
  ( EExpr(..)
  , Atom(..)
  , Combiner(..)
  , location
  , pattern StrAtom
  -- , Phase(..)
  , Error(..)
  , NonEmpty2(..)
  ) where

import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Text.EExpr.Tokens.Types (Location)
import Text.Lightyear (TextPos)

import qualified Text.EExpr.Tokens.Types as Tok


type Token = Tok.Token 'Tok.Sens

-- WARNING: There are plenty of forms that the parser is unable to construct
-- I'd much prefer a more accurate type, but I think w/o a dependently typed lang, it's not worth the indexing
data EExpr :: Type where
  Atom      :: Location
            -> Atom
            -> EExpr
  Combine   :: Location
            -> Combiner t_subexprs
            -> t_subexprs
            -> EExpr
  -- TODO errors, only available in a phase

data Atom
  = IntAtom Text Integer
  | RatAtom Text Rational
  | SymAtom Text

data Combiner :: Type -> Type where
  -- strings
  StrTempl :: Combiner (Text, [(EExpr, Text)])
  -- enclosers
  Paren :: Combiner (Maybe EExpr)
  Bracket :: Combiner (Maybe EExpr)
  Brace :: Combiner (Maybe EExpr)
  Indent :: Combiner (NonEmpty EExpr)
  -- separators
  Apply :: Combiner (NonEmpty EExpr)
  Ellipsis :: Combiner (Maybe EExpr, Maybe EExpr)
  Colon :: Combiner (EExpr, EExpr)
  Comma :: Combiner [EExpr]
  Semicolon :: Combiner [EExpr]
  -- other
  Chain :: Combiner (NonEmpty2 EExpr)
  SyntheticDot :: Combiner EExpr
  Mixfix :: Combiner (EExpr, [EExpr]) -- TODO or should I have a non-empty list?
    -- TODO ^ after mixfix processing is another phase


pattern StrAtom :: Location -> Text -> EExpr
pattern StrAtom a str = Combine a StrTempl (str, [])


location :: EExpr -> Location
location (Atom l _) = l
location (Combine l _ _) = l


------ Errors ------

data Error
    = Unexpected TextPos (Maybe Token) [String] -- unexpected character/end-of-input, expected set
    -- TODO
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
    -- TODO when multiple `Unexpected`s occur at same location, merge expectation list


------ Helper ------

infixr 5 :||
data NonEmpty2 a = (a, a) :|| [a]
