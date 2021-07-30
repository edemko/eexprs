{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.List.Reverse
  ( RList(..)
  , nil
  , snoc
  , null
  , toList
  , toSet
  ) where

import Prelude hiding (null)

import Data.Set (Set)

import qualified Data.List as List
import qualified Data.Set as Set


newtype RList a = RList [a]
  deriving stock (Show)
  deriving newtype (Functor)

nil :: RList a
nil = RList []

snoc :: RList a -> a -> RList a
snoc (RList xs) x = RList (x:xs)

null :: RList a -> Bool
null (RList xs) = List.null xs

toList :: RList a -> [a]
toList (RList xs) = reverse xs

toSet :: (Ord a) => RList a -> Set a
toSet (RList xs) = Set.fromList xs
