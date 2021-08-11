{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | I dunno why HList has such tight bounds on base, so I guess I just have to write my own.
-- It doesn't help that when I look for type-level list stuff I get tutorials instead of libraries.
module Data.NTuple
  ( NTuple(..)
  , Map
  )
  where

infixr 5 :::
data NTuple (ts :: [*]) where
  TNil :: NTuple '[]
  (:::) :: t -> NTuple ts -> NTuple (t ': ts)

type family Map f xs where
  Map f '[] = '[]
  Map f (x ': xs) = f x ': Map f xs
