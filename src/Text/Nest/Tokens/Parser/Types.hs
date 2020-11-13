{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

module Text.Nest.Tokens.Parser.Types
  ( Nest(..)
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
import Text.Nest.Tokens.Types (Location)

import qualified Text.Nest.Tokens.Types as Tok


-- FIXME
-- I expect that nested separators will be used uniformly
-- that is, if two terms are semi-colon-separated, and the first term is comma-separated, then the second term should also be comma-separated
-- this should probably be enforced in the type system if I can manage it
-- otherwise, I'd want a uniformicity pass
-- 
-- ultimately, this scheme wuold be due to the idea that separators "belong" to their enclosers


type Token = Tok.Token 'Tok.Sens

data Phase = Correct | Recovered

-- WARNING: There are plenty of forms that the parser is unable to construct
-- I'd much prefer a more accurate type, but I think w/o a dependently typed lang, it's not worth the indexing
data Nest :: Phase -> Type where
  Ellipsis  :: Location -> Nest phase
  NilAtom   :: Location
            -> Surrounder 'Inline
            -> Maybe (Punctuation anyListy 'Inline)
            -> Nest phase
  IntAtom   :: Location -> Text -> Integer -> Nest phase
  RatAtom   :: Location -> Text -> Rational -> Nest phase
  SymAtom   :: Location -> Text -> Nest phase
  -- combinations
  StrTempl  :: Location
            -> Text -> [(Nest phase, Text)]
            -> Nest phase
  Surround  :: Location
            -> Surrounder anyInline
            -> Nest phase
            -> Nest phase
  Separate  :: Location
            -> Punctuation anyListy anyInline
            -> Split anyListy (Nest phase) -- FIXME NonEmpty
            -> Nest phase
  Combine   :: Location -> [Nest phase] -> Nest phase
  Chain     :: Location -> Nakedness -> [Nest phase] -> Nest phase
  -- errors
  Error     :: Error -> Nest 'Recovered

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

pattern StrAtom :: Location -> Text -> Nest st
pattern StrAtom a str = StrTempl a str []


location :: Nest ph -> Location
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
