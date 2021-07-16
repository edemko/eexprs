{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.List.Reverse
  ( RList(..)
  , nil
  , snoc
  , toList
  , toSet
  ) where

import Data.Set (Set)

import qualified Data.Set as Set


newtype RList a = RList [a]
  deriving (Functor)

nil :: RList a
nil = RList []

snoc :: RList a -> a -> RList a
snoc (RList xs) x = RList (x:xs)

toList :: RList a -> [a]
toList (RList xs) = reverse xs

toSet :: (Ord a) => RList a -> Set a
toSet (RList xs) = Set.fromList xs
