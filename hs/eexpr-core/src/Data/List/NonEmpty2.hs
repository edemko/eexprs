{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}

module Data.List.NonEmpty2
  ( NonEmpty2(..)
  -- * Basic Functions
  , length
  , uncons
  , head
  , tail
  , init
  , last
  , doubleton
  , (<||)
  -- * Converting to and from a list
  , toList
  , fromList
  , nonEmpty2
  -- * Re-exports
  , NonEmpty(..)
  ) where

import Prelude hiding (length,head,tail,init,last)
import qualified Prelude as Prelude

import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty(..),(<|))

import qualified Data.List.NonEmpty as NonEmpty


infixr 5 :||
data NonEmpty2 a = a :|| NonEmpty a
  deriving stock (Eq, Ord)
  deriving stock (Functor)
  deriving stock (Read, Show)

instance Foldable NonEmpty2 where
  foldMap f (x :|| xs) = f x <> foldMap f xs

instance Traversable NonEmpty2 where
  traverse f (x :|| xs) = (:||) <$> f x <*> traverse f xs

instance Semigroup (NonEmpty2 a) where
  (a1 :|| a2 :| as) <> bs = a1 :|| a2 :| as <> toList bs


length :: NonEmpty2 a -> Int
length (_ :|| _ :| xs) = 2 + Prelude.length xs

uncons :: NonEmpty2 a -> (a, NonEmpty a)
uncons (x :|| xs) = (x, xs)

head :: NonEmpty2 a -> a
head (x :|| _) = x

tail :: NonEmpty2 a -> NonEmpty a
tail (_ :|| xs) = xs

init :: NonEmpty2 a -> NonEmpty a
init (x :|| xs) = x :| NonEmpty.init xs

last :: NonEmpty2 a -> a
last (_ :|| x :| []) = x
last (_ :|| _ :| xs) = Prelude.last xs

doubleton :: a -> a -> NonEmpty2 a
doubleton x y = x :|| y :| []

infixr 5 <||
(<||) :: a -> NonEmpty2 a -> NonEmpty2 a
x <|| (y :|| ys) = x :|| y <| ys


fromList :: [a] -> NonEmpty2 a
fromList (x:y:zs) = x :|| y :| zs
fromList [_] = errorWithoutStackTrace "NonEmpty2.fromList: singleton list"
fromList [] = errorWithoutStackTrace "NonEmpty2.fromList: empty list"

nonEmpty2 :: [a] -> Maybe (NonEmpty2 a)
nonEmpty2 (x:y:zs) = Just (x :|| y :| zs)
nonEmpty2 _ = Nothing
