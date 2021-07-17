{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}

module Data.Eexpr.Grammar.Internal
  ( Grammar(..)
  , Context
  , Error(..)
  , runGrammar
  , liftEither
  , liftMaybe
  , fromMaybe
  , context
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
import Data.Eexpr.Types (Eexpr(..))
import Data.Either (partitionEithers)
import Data.Foldable (Foldable(..))
import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty(..))
import Data.Profunctor(Profunctor(..))


-- | 'Grammar' presents an interface to pattern-match 'Eexpr's with error reporting.
-- It maintains a context of the ancestor 'Eexpr's which can help with meaningful error reporting.
-- However, there is no need for a 'Grammar' to match only 'Eexpr' values, as there may be many intermediate values.
-- You have the option to choose your own error type; these errors should have rich semantics so that they can be rendered in any later-required format.
--
-- I've chosen to expose 'Grammar' primarily as an 'Arrow'.
-- Exposing a 'Monad' directly would make it too easy to mismanage context information, thus creating unacepptably inaccurate error messages.
newtype Grammar ann err a b = Grammar
  { unGrammar :: Context ann -> a -> Either (NonEmpty (Error ann err)) (Context ann, b) }

type Context ann = NonEmpty (Eexpr ann)

data Error ann err = Error (Context ann) err
  deriving stock (Read, Show)
  deriving stock (Functor)

runGrammar :: Grammar ann err (Eexpr ann) a -> Eexpr ann -> Either (NonEmpty (Error ann err)) a
runGrammar g e = snd <$> unGrammar g (e :| []) e

context :: Grammar ann err a (Context ann)
context = Grammar $ \ctx _ -> Right (ctx, ctx)

liftEither :: (a -> Either err b) -> Grammar ann err a b
liftEither f = Grammar $ \ann a -> case f a of
  Right b -> Right (ann, b)
  Left err -> Left $ Error ann err :| []

liftMaybe :: err -> (a -> Maybe b) -> Grammar ann err a b
liftMaybe err f = liftEither $ \a -> maybe (Left err) Right (f a)

fromMaybe :: err -> Grammar ann err a (Maybe b) -> Grammar ann err a b
fromMaybe err g = g >>> liftMaybe err id

instance Functor (Grammar ann err a) where
  fmap f (Grammar g) = Grammar $ \ctx a -> case g ctx a of
    Right (_, b) -> Right (ctx, f b)
    Left err -> Left err

instance Profunctor (Grammar ann err) where
  lmap f (Grammar g) = Grammar $ \ctx a -> g ctx (f a)
  rmap f g = f <$> g

instance Applicative (Grammar ann err a) where
  pure x = Grammar $ \ctx -> const $ Right (ctx, x)
  (Grammar g1) <*> (Grammar g2) = Grammar $ \ctx a ->
    case (g1 ctx a, g2 ctx a) of
      (Right (_, f), Right (_, x)) -> Right (ctx, f x)
      (Left e1, Right _) -> Left e1
      (Right _, Left e2) -> Left e2
      (Left e1, Left e2) -> Left $ e1 <> e2

-- Together, choice and fail are essentially `Alternative`, but they require an extra `err` type argument

choice :: (Foldable f) => err -> f (Grammar ann any a b) -> Grammar ann err a b
choice err alts = Grammar $ \ctx a -> go ctx a (toList alts)
  where
  go ctx a (Grammar g:gs) = case g ctx a of
    Right b -> Right b
    Left _ -> go ctx a gs
  go ctx a [] = unGrammar (fail err) ctx a

fail :: err -> Grammar ann err a b
fail err = Grammar $ \ctx _ -> Left $ Error ctx err :| []

instance Category (Grammar ann err) where
  id = Grammar $ \ctx a -> Right (ctx, a)
  (Grammar g') . (Grammar g) = Grammar $ \ctx a ->
    case g ctx a of
      Right (ctx', b) -> g' ctx' b
      Left err -> Left err

instance Arrow (Grammar ann err) where
  arr f = Grammar $ \ctx a -> Right (ctx, f a)
  (Grammar g) *** (Grammar g') = Grammar $ \ctx (a, a') ->
    case (g ctx a, g' ctx a') of
      (Right (_, b), Right (_, b')) -> Right (ctx, (b, b'))
      (Left e1, Right _) -> Left e1
      (Right _, Left e2) -> Left e2
      (Left e1, Left e2) -> Left $ e1 <> e2

instance ArrowApply (Grammar ann err) where
  app = Grammar $ \ctx (Grammar g, x) -> g ctx x

map :: Grammar ann err a b -> Grammar ann err [a] [b]
map (Grammar g) = Grammar $ \ctx as -> g ctx <$> as & partitionEithers & \case
  ([], bs) -> Right (ctx, snd <$> bs)
  (e:es, _) -> Left $ join (e :| es)

mapErrors :: (err -> err') -> Grammar ann err a b -> Grammar ann err' a b
mapErrors f (Grammar g) = Grammar $ \ctx x -> case g ctx x of
  Right it -> Right it
  Left errs -> Left (fmap f <$> errs)
