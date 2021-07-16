module Numeric.Interval.Compare
  ( Overlapping(..)
  , Orderment(..)
  , Containment(..)
  , overlap
  ) where

import Numeric.Interval


data Overlapping
  = Equal
  | Disjoint Orderment
  | Tangent Orderment
  | Contained Containment
  | Osculating Orderment Containment
  | Staggered Orderment

data Orderment
  = Earlier
  | Later
data Containment
  = Smaller
  | Larger

overlap :: (Ord a) => Interval a -> Interval a -> Overlapping
overlap a b = case (inf a `compare` inf b, sup a `compare` sup b) of
  (EQ, EQ) -> Equal
  (EQ, LT) -> Osculating Earlier Smaller
  (EQ, GT) -> Osculating Earlier Larger
  (LT, EQ) -> Osculating Later Larger
  (GT, EQ) -> Osculating Later Smaller
  (GT, LT) -> Contained Smaller
  (LT, GT) -> Contained Larger
  (LT, LT) -> case sup a `compare` inf b of
    LT -> Disjoint Earlier
    EQ -> Tangent Earlier
    GT -> Staggered Earlier
  (GT, GT) -> case sup b `compare` inf a of
    LT -> Disjoint Later
    EQ -> Tangent Later
    GT -> Staggered Later
