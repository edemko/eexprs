{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}

module Data.Eexpr.Grammar.Internal
  ( Grammar(..)
  , GrammarError(..)
  , runGrammar
  , liftEither
  , liftMaybe
  , fromMaybe
  , location
  , map
  , mapErrors
  -- * Altered Alternative
  , choice
  , fail
  ) where

import Prelude hiding (id,map,fail)

import Control.Arrow (Arrow(..),ArrowApply(..),(>>>))
import Control.Category (Category(..))
import Control.Monad (join)
import Data.Eexpr.Types (Eexpr(..), annotation, Location(..))
import Data.Either (partitionEithers)
import Data.Foldable (Foldable(..))
import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty(..))
import Data.Profunctor(Profunctor(..))

newtype Grammar err a b = Grammar
  { unGrammar :: Location -> a -> Either (NonEmpty (GrammarError err)) (Location, b) }

data GrammarError err = GrammarError !Location err
  deriving stock (Read, Show)
  deriving stock (Functor)

runGrammar :: Grammar err (Eexpr Location) a -> Eexpr Location -> Either (NonEmpty (GrammarError err)) a
runGrammar g e = snd <$> unGrammar g (annotation e) e

location :: Grammar err a Location
location = Grammar $ \l _ -> Right (l, l)

liftEither :: (a -> Either err b) -> Grammar err a b
liftEither f = Grammar $ \l a -> case f a of
  Right b -> Right (l, b)
  Left err -> Left $ GrammarError l err :| []

liftMaybe :: err -> (a -> Maybe b) -> Grammar err a b
liftMaybe err f = liftEither $ \a -> maybe (Left err) Right (f a)

fromMaybe :: err -> Grammar err a (Maybe b) -> Grammar err a b
fromMaybe err g = g >>> liftMaybe err id

instance Functor (Grammar err a) where
  fmap f (Grammar g) = Grammar $ \l a -> case g l a of
    Right (_, b) -> Right (l, f b)
    Left err -> Left err

instance Profunctor (Grammar err) where
  lmap f (Grammar g) = Grammar $ \l a -> g l (f a)
  rmap f g = f <$> g

instance Applicative (Grammar err a) where
  pure x = Grammar $ \l -> const $ Right (l, x)
  (Grammar g1) <*> (Grammar g2) = Grammar $ \l a ->
    case (g1 l a, g2 l a) of
      (Right (_, f), Right (_, x)) -> Right (l, f x)
      (Left e1, Right _) -> Left e1
      (Right _, Left e2) -> Left e2
      (Left e1, Left e2) -> Left $ e1 <> e2

-- Together, choice and fail are essentially `Alternative`, but they require an extra `err` type argument

choice :: (Foldable f) => err -> f (Grammar any a b) -> Grammar err a b
choice err alts = Grammar $ \l a -> go l a (toList alts)
  where
  go l a (Grammar g:gs) = case g l a of
    Right b -> Right b
    Left _ -> go l a gs
  go l a [] = unGrammar (fail err) l a

fail :: err -> Grammar err a b
fail err = Grammar $ \l _ -> Left $ GrammarError l err :| []

instance Category (Grammar err) where
  id = Grammar $ \l a -> Right (l, a)
  (Grammar g') . (Grammar g) = Grammar $ \l a ->
    case g l a of
      Right (l', b) -> g' l' b
      Left err -> Left err

instance Arrow (Grammar err) where
  arr f = Grammar $ \l a -> Right (l, f a)
  (Grammar g) *** (Grammar g') = Grammar $ \l (a, a') ->
    case (g l a, g' l a') of
      (Right (_, b), Right (_, b')) -> Right (l, (b, b'))
      (Left e1, Right _) -> Left e1
      (Right _, Left e2) -> Left e2
      (Left e1, Left e2) -> Left $ e1 <> e2

instance ArrowApply (Grammar err) where
  app = Grammar $ \l (Grammar g, x) -> g l x

map :: Grammar err a b -> Grammar err [a] [b]
map (Grammar g) = Grammar $ \l as -> g l <$> as & partitionEithers & \case
  ([], bs) -> Right (l, snd <$> bs)
  (e:es, _) -> Left $ join (e :| es)

mapErrors :: (err -> err') -> Grammar err a b -> Grammar err' a b
mapErrors f (Grammar g) = Grammar $ \l x -> case g l x of
  Right it -> Right it
  Left errs -> Left (fmap f <$> errs)
