{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS -fno-warn-orphans #-}
module Text.Nest.Tokens.Stream where

import Prelude hiding (length)

import Data.Sequence (Seq, ViewL(..), ViewR(..))
import Text.Lightyear.Position (startTextPos)
import Text.Lightyear.Stream (Stream(..))
import Text.Nest.Tokens.Types.Narrow (Token(..), Location(..))

import qualified Data.Sequence as Seq

instance Stream (Seq Token) where
  type Chr (Seq Token) = Token
  type Pos (Seq Token) = Location
  uncons xs = case Seq.viewl xs of
    EmptyL -> Nothing
    x :< xs' -> Just (x, xs')
  splitN n xs =
    let (prefix, suffix) = Seq.splitAt n xs
     in if (Seq.length prefix < n)
        then Nothing
        else Just (prefix, suffix)
  splitPred = Seq.spanl
  length = Seq.length
  startPosition _ = Loc
    { file = "<unknown>"
    , from = startTextPos
    , to = startTextPos
    }
  advanceOne _ T{loc} _ = loc
  advance xs loc0 = case Seq.viewr xs of
    EmptyR -> loc0
    _ :> T{loc} -> loc
