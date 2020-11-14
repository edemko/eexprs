{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

module Text.EExpr.Tokens.Parser.Types
  ( EExpr(..)
  , Surrounder(..)
  , Punctuation(..)
  , Listy(..)
  , Inline(..)
  , Nakedness(..)
  , location
  , pattern StrAtom
  , Phase(..)
  , Error(..)
  ) where

import Data.Kind (Type)
import Data.Text (Text)
import Text.Lightyear (TextPos)
import Text.EExpr.Tokens.Types (Location)

import qualified Text.EExpr.Tokens.Types as Tok


type Token = Tok.Token 'Tok.Sens

data Phase = Correct | Recovered

-- WARNING: There are plenty of forms that the parser is unable to construct
-- I'd much prefer a more accurate type, but I think w/o a dependently typed lang, it's not worth the indexing
data EExpr :: Phase -> Type where
  Ellipsis  :: Location -> EExpr phase
  NilAtom   :: Location
            -> Surrounder 'Inline
            -> Maybe (Punctuation anyListy 'Inline)
            -> EExpr phase
  IntAtom   :: Location -> Text -> Integer -> EExpr phase
  RatAtom   :: Location -> Text -> Rational -> EExpr phase
  SymAtom   :: Location -> Text -> EExpr phase
  -- combinations
  StrTempl  :: Location
            -> Text -> [(EExpr phase, Text)]
            -> EExpr phase
  Surround  :: Location
            -> Surrounder anyInline
            -> EExpr phase
            -> EExpr phase
  Separate  :: Location
            -> Punctuation anyListy anyInline
            -> Split anyListy (EExpr phase) -- FIXME NonEmpty
            -> EExpr phase
  Combine   :: Location -> [EExpr phase] -> EExpr phase
  Chain     :: Location -> Nakedness -> [EExpr phase] -> EExpr phase
  -- errors
  Error     :: Error -> EExpr 'Recovered

data Nakedness = Standard | Naked
data Listy = Listy | Pairy
data Inline = Inline | NotInline

data Punctuation :: Listy -> Inline -> Type where
  Colon :: Punctuation 'Pairy 'Inline
  Comma :: Punctuation 'Listy 'Inline
  Semicolon :: Punctuation 'Listy 'Inline
  Newline :: Punctuation 'Listy 'NotInline

data Surrounder :: Inline -> Type where
  Paren :: Surrounder 'Inline
  Bracket :: Surrounder 'Inline
  Brace :: Surrounder 'Inline
  Indent :: Surrounder 'NotInline

type family Split (l :: Listy) a :: Type where
  Split 'Listy a = [a]
  Split 'Pairy a = (a, a)

pattern StrAtom :: Location -> Text -> EExpr st
pattern StrAtom a str = StrTempl a str []


location :: EExpr ph -> Location
location (Ellipsis l) = l
location (NilAtom l _ _) = l
location (IntAtom l _ _) = l
location (RatAtom l _ _) = l
location (SymAtom l _) = l
location (StrTempl l _ _) = l
location (Surround l _ _) = l
location (Separate l _ _) = l
location (Combine l _) = l
location (Chain l _ _) = l
location (Error _) = error "TODO call `location` on `Error`"


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
