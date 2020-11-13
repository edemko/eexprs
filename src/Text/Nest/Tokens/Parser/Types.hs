{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternSynonyms #-}

module Text.Nest.Tokens.Parser.Types
  ( Nest(..)
  , Encloser(..)
  , Separator(..)
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
  NilAtom   :: Location -> Encloser                     -> Nest phase
  -- TODO what about `{:}`? I guess I need a colon-styled nil
  IntAtom   :: Location -> Text -> Integer              -> Nest phase
  RatAtom   :: Location -> Text -> Rational             -> Nest phase
  SymAtom   :: Location -> Text                         -> Nest phase
  StrTempl  :: Location -> Text -> [(Nest phase, Text)] -> Nest phase
  Ellipsis  :: Location                                 -> Nest phase
  -- combinations
  Enclose   :: Location -> Encloser -> Nest phase       -> Nest phase
  Indent    :: Location -> [Nest phase]                 -> Nest phase
  Separate  :: Location -> Separator -> [Nest phase]    -> Nest phase
  Colon     :: Location -> Nest phase -> Nest phase     -> Nest phase
  Combine   :: Location -> [Nest phase]                 -> Nest phase
  PrefixDot :: Location -> Nest phase                   -> Nest phase
  Chain     :: Location -> [Nest phase]                 -> Nest phase
  -- errors
  Error     :: Error                                    -> Nest 'Recovered

data Encloser
  = Paren
  | Bracket
  | Brace
  deriving(Show)

data Separator
  = Comma
  | Semicolon
  deriving(Show)

pattern StrAtom :: Location -> Text -> Nest st
pattern StrAtom a str = StrTempl a str []


location :: Nest ph -> Location
location (NilAtom l _) = l
location (IntAtom l _ _) = l
location (RatAtom l _ _) = l
location (SymAtom l _) = l
location (StrTempl l _ _) = l
location (Ellipsis l) = l
location (Enclose l _ _) = l
location (Indent l _) = l
location (Separate l _ _) = l
location (Colon l _  _) = l
location (Combine l _) = l
location (PrefixDot l _) = l
location (Chain l _) = l
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
