{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms #-}

module Data.Eexpr.Types
  ( Eexpr(..)
  , Bignum(..)
  , Radix(Radix, Base2, Base8, Base10, Base12, Base16)
  , fromBase
  , Location(..)
  , LocPoint(..)
  ) where

import Data.List.NonEmpty2 (NonEmpty, NonEmpty2)
import Data.Text.Short (ShortText)
import Data.Word (Word8,Word32)

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
