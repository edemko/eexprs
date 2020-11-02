{-# LANGUAGE PatternSynonyms #-}

module Data.Nest.Types
  ( Nest(..)
  , Encloser(..)
  , Separator(..)
  , pattern StrAtom
  ) where

import Data.Text (Text)


data Nest ann
  = IntAtom  ann Integer
  | RatAtom  ann Rational
  | SymAtom  ann Text
  | StrTempl ann Text [(Nest ann, Text)]
  | Enclose  ann Encloser (Nest ann)
  | Separate ann Separator [Nest ann]
  | Indent   ann [Nest ann]
  | Chain    ann [Nest ann]
  deriving(Show)

data Encloser
  = Paren
  | Bracket
  | Brace
  | PrefixDot
  deriving(Show)

data Separator
  = Space
  | Comma
  | Dot
  | Ellipsis
  | Semicolon
  | Colon
  deriving(Show)

pattern StrAtom :: ann -> Text -> Nest ann
pattern StrAtom a str = StrTempl a str []
