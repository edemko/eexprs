{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms #-}

module Data.Eexpr.Types
  ( Eexpr(..)
  , mapAnnotation
  , Bignum(..)
  , Radix(Radix, Base2, Base8, Base10, Base12, Base16)
  , fromBase
  , Location(..)
  , LocPoint(..)
  , Error(..)
  ) where

import Data.Bifunctor (first)
import Data.List.NonEmpty2 (NonEmpty, NonEmpty2)
import Data.Text.Short (ShortText)
import Data.Word (Word8,Word32)


------------------------ Expressions ------------------------

data Eexpr ann
  = Symbol    ann ShortText
  | Number    ann Bignum
  | String    ann ShortText [(Eexpr ann, ShortText)]
  | Paren     ann (Maybe (Eexpr ann))
  | Bracket   ann (Maybe (Eexpr ann))
  | Brace     ann (Maybe (Eexpr ann))
  | Block     ann (NonEmpty (Eexpr ann))
  | Predot    ann (Eexpr ann)
  | Chain     ann (NonEmpty2 (Eexpr ann))
  | Space     ann (NonEmpty2 (Eexpr ann))
  | Ellipsis  ann (Maybe (Eexpr ann)) (Maybe (Eexpr ann))
  | Colon     ann (Eexpr ann) (Eexpr ann)
  | Comma     ann [Eexpr ann]
  | Semicolon ann [Eexpr ann]
  deriving stock (Read, Show)

mapAnnotation :: (a -> b) -> Eexpr a -> Eexpr b
mapAnnotation f (Symbol a x) = Symbol (f a) x
mapAnnotation f (Number a n) = Number (f a) n
mapAnnotation f (String a hd tl) = String (f a) hd (first (mapAnnotation f) <$> tl)
mapAnnotation f (Paren a sub) = Paren (f a) (mapAnnotation f <$> sub)
mapAnnotation f (Bracket a sub) = Bracket (f a) (mapAnnotation f <$> sub)
mapAnnotation f (Brace a sub) = Brace (f a) (mapAnnotation f <$> sub)
mapAnnotation f (Block a subs) = Block (f a) (mapAnnotation f <$> subs)
mapAnnotation f (Predot a sub) = Predot (f a) (mapAnnotation f sub)
mapAnnotation f (Chain a subs) = Chain (f a) (mapAnnotation f <$> subs)
mapAnnotation f (Space a subs) = Space (f a) (mapAnnotation f <$> subs)
mapAnnotation f (Ellipsis a l r) = Ellipsis (f a) (mapAnnotation f <$> l) (mapAnnotation f <$> r)
mapAnnotation f (Colon a l r) = Colon (f a) (mapAnnotation f l) (mapAnnotation f r)
mapAnnotation f (Comma a subs) = Comma (f a) (mapAnnotation f <$> subs)
mapAnnotation f (Semicolon a subs) = Semicolon (f a) (mapAnnotation f <$> subs)


------------------------ Helper Types ------------------------

data Bignum = Bignum
  { significand :: Integer
  , radix :: {-# UNPACK #-} !Radix
  , fractionalExponent :: Word32
  , explicitExponent :: Integer
  }
  deriving stock (Read, Show)

newtype Radix = Radix Word8
  deriving stock (Read, Show)
  deriving stock (Eq, Ord)

{-# COMPLETE Base2, Base8, Base10, Base12, Base16 #-}
pattern Base2 :: Radix
pattern Base2 = Radix 2
pattern Base8 :: Radix
pattern Base8 = Radix 8
pattern Base10 :: Radix
pattern Base10 = Radix 10
pattern Base12 :: Radix
pattern Base12 = Radix 12
pattern Base16 :: Radix
pattern Base16 = Radix 16

fromBase :: (Integral n) => Radix -> n
fromBase (Radix r) = fromIntegral r


------------------------ Location ------------------------

data Location = Location
  { start :: {-# UNPACK #-} !LocPoint
  , end :: {-# UNPACK #-} !LocPoint
  }
  deriving stock(Read, Show)
data LocPoint = LocPoint
  { byteOff :: {-# UNPACK #-} !Word
  , lineOff :: {-# UNPACK #-} !Word
  , colOff :: {-# UNPACK #-} !Word
  }
  deriving stock(Read, Show)


------------------------ Errors and Warnings ------------------------

-- TODO some of these errors could use additional info
data Error
  = BadBytes !Location
  | BadChar !Location
  | MixedSpace !Location
  | MixedNewlines !Location
  | BadDigitSeparator !Location
  | MissingExponent !Location
  | BadExponentSign !Location
  | BadEscapeChar !Location
  | BadEscapeCode !Location
  | UnicodeOverflow !Location
  | BadStringChar !Location
  | MissingLinePickup !Location
  | UnclosedString !Location
  | UnclosedMultilineString !Location
  | HeredocBadOpen !Location
  | HeredocBadIndentDefinition !Location
  | HeredocBadIndentation !Location
  | MixedIndentation !Location
  | TrailingSpace !Location
  | NoTrailingNewline !Location
  | ShallowIndent !Location
  | Offsides !Location
  | BadDot !Location
  | CrammedTokens !Location
  | UnbalancedWrap !Location
  | ExpectingNewlineOrDedent !Location
  | MissingTemplateExpr !Location
  | MissingCloseTemplate !Location
  deriving stock (Read, Show)
