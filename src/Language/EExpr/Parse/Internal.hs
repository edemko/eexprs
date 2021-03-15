module Language.EExpr.Parse.Internal
  ( Parser(..)
  , run
  , Error(..)
  , arr, (>->)
  , fail
  , message
  , NonEmpty((:|))
  ) where

import Prelude hiding (fail)

import Control.Applicative (Alternative(..))
import Data.List.NonEmpty (NonEmpty((:|)))
import Language.EExpr.Types (Location)

newtype Parser a b = P
  { unP :: Location -> a -> Either (NonEmpty Error) (Location, b)
  }

data Error
  = Error Location String
  | AmbErrors [NonEmpty Error]


run :: Parser a b -> Location -> a -> Either (NonEmpty Error) b
run (P p) loc0 x = case p loc0 x of
  Left errs -> Left errs
  Right (_, y) -> Right y

instance Functor (Parser a) where
  fmap f (P p) = P $ \loc x -> case p loc x of
    Left errs -> Left errs
    Right (loc', y) -> Right (loc', f y)

instance Applicative (Parser a) where
  pure x = P $ \loc _ -> Right (loc, x)
  (P a) <*> (P b) = P $ \loc0 x -> case a loc0 x of
    Left errs -> Left errs
    Right (_, f) -> case b loc0 x of
      Left errs -> Left errs
      Right (locG, y) -> Right (locG, f y)

instance Alternative (Parser a) where
  empty = P $ \loc _ -> Left $ Error loc "no options expected (Alternative.empty)" :| []
  (P a) <|> (P b) = P $ \loc0 x -> case a loc0 x of
    Right it -> Right it
    Left errsA -> case b loc0 x of
      Right it -> Right it
      Left errsB -> Left errsB -- TODO I'd prefer to combine the two error sets

arr :: (a -> Either String b) -> Parser a b
arr f = P $ \loc0 x -> case f x of
  Left msg -> Left $ Error loc0 msg :| []
  Right y -> Right (loc0, y)

(>->) :: Parser a b -> Parser b c -> Parser a c
(P a) >-> (P b) = P $ \loc0 x -> case a loc0 x of
  Left errs -> Left errs
  Right (locA, y) -> b locA y

fail :: String -> Parser a b
fail msg = P $ \loc _ -> Left $ Error loc msg :| []

message :: String -> Parser a b -> Parser a b
message msg (P p) = P $ \loc a -> case p loc a of
  Right it -> Right it
  Left _ -> Left $ Error loc msg :| []
