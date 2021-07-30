{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Eexpr.Grammar.Internal
  ( Grammar(..)
  , InnerResult
  , Context
  , Errors
  , Error(..)
  , runGrammar
  , liftEither
  , liftMaybe
  , fromMaybe
  , context
  , map
  , mapErrors
  , ZipGrammar(..)
  , zip1
  , zip2
  , zip3
  -- * Altered Alternative
  , choice
  , fail
  ) where

import Prelude hiding (id,map,zip3,fail)

import Control.Arrow (Arrow(..),ArrowChoice(..),ArrowApply(..),(>>>))
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
  { unGrammar :: Context ann -> a -> InnerResult ann err b }
type InnerResult ann err a = Either (Errors ann err) (Context ann, a)

type Context ann = NonEmpty (Eexpr ann)

type Errors ann err = NonEmpty (Error ann err)
data Error ann err = Error (Context ann) err
  deriving stock (Read, Show)
  deriving stock (Functor)

runGrammar :: Grammar ann err (Eexpr ann) a -> Eexpr ann -> Either (Errors ann err) a
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
  go ctx _ [] = innerFail ctx err

fail :: err -> Grammar ann err a b
fail err = Grammar $ \ctx _ -> Left $ Error ctx err :| []

innerFail :: Context ann -> err -> InnerResult ann err any
innerFail ctx err = Left $ Error ctx err :| []

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

instance ArrowChoice (Grammar ann err) where
  (Grammar gl) +++ (Grammar gr) = Grammar $ \ctx -> \case
    Left xl -> case gl ctx xl of
      Right (ctx', l) -> Right (ctx', Left l)
      Left err -> Left err
    Right xr -> case gr ctx xr of
      Right (ctx', r) -> Right (ctx', Right r)
      Left err -> Left err

instance ArrowApply (Grammar ann err) where
  app = Grammar $ \ctx (Grammar g, x) -> g ctx x

map :: Foldable t => Grammar ann err a b -> Grammar ann err (t a) [b]
map (Grammar g) = Grammar $ \ctx as -> g ctx <$> toList as
  & partitionEithers & \case
    ([], bs) -> Right (ctx, snd <$> bs)
    (e:es, _) -> Left $ join (e :| es)

data ZipGrammar ann err a b = ZG (Grammar ann err a b) err

zip1 :: ZipGrammar ann err a b1
        -- ^ grammar for the first given expression
     -> (a -> err) -- ^ create error when more exprs are given than expected
     -> Grammar ann err [a] b1
zip1 (ZG g1 err1) unexpected = Grammar $ \ctx -> \case
  [] -> innerFail ctx err1
  [a1] -> unGrammar g1 ctx a1
  a1 : a2 : _ ->
    let err = Error ctx (unexpected a2) :| []
     in case unGrammar g1 ctx a1 of
          Right _ -> Left err
          Left errs -> Left $ errs <> err

zip2 :: (ZipGrammar ann err a b1, ZipGrammar ann err a b2)
        -- ^ grammars for the frist two given expression
     -> (a -> err) -- ^ create error when more exprs are given than expected
     -> Grammar ann err [a] (b1, b2)
zip2 (ZG g1 err1, zg2) unexpected = Grammar $ \ctx -> \case
  [] -> innerFail ctx err1
  a1 : rest -> case (unGrammar g1 ctx a1, unGrammar (zip1 zg2 unexpected) ctx rest) of
    (Right (_, b1), Right (_, b2)) -> Right (ctx, (b1, b2))
    (Left err, Left errs) -> Left $ err <> errs
    (Left err, _) -> Left err
    (_, Left errs) -> Left errs

zip3 :: (ZipGrammar ann err a b1, ZipGrammar ann err a b2, ZipGrammar ann err a b3)
        -- ^ grammars for the frist two given expression
     -> (a -> err) -- ^ create error when more exprs are given than expected
     -> Grammar ann err [a] (b1, b2, b3)
zip3 (ZG g1 err1, zg2, zg3) unexpected = Grammar $ \ctx -> \case
  [] -> innerFail ctx err1
  a1 : rest -> case (unGrammar g1 ctx a1, unGrammar (zip2 (zg2, zg3) unexpected) ctx rest) of
    (Right (_, b1), Right (_, (b2, b3))) -> Right (ctx, (b1, b2, b3))
    (Left err, Left errs) -> Left $ err <> errs
    (Left err, _) -> Left err
    (_, Left errs) -> Left errs


-- zip :: forall f ann err a bs. (Foldable f)
--     => NTuple (Map (ZipGrammar ann err a) bs)
--     -> (a -> err)
--     -> Grammar ann err (f a) (NTuple bs)
-- zip gs0 unexpected = Grammar $ \ctx as0 ->
--   let loop :: forall bs'. NTuple (Map (ZipGrammar ann err a) bs') -> [a] -> Either (Errors ann err) (NTuple bs')
--       loop (ZG g missing ::: gs) (a : as) = case unGrammar g ctx a of
--         Right (_, b) -> case loop gs as of
--           Right bs -> Right $ b ::: bs
--           Left errs -> Left errs
--         Left err -> case loop gs as of
--           Right _ -> Left err
--           Left errs -> Left $ err <> errs
--       loop TNil [] = Right TNil
--    in case loop gs0 (toList as0) of
--         Right bs -> Right (ctx, bs)
--         Left errs -> Left errs

mapErrors :: (err -> err') -> Grammar ann err a b -> Grammar ann err' a b
mapErrors f (Grammar g) = Grammar $ \ctx x -> case g ctx x of
  Right it -> Right it
  Left errs -> Left (fmap f <$> errs)
