{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS -fno-warn-orphans #-}
module Text.Nest.Tokens.Stream
  ( LexStream
  , LexElem(..)
  , mkStream
  ) where

import Prelude hiding (length)
import Text.Nest.Tokens.Types

import Data.Text (Text)
import Data.Sequence (Seq, (|>), ViewL(..), ViewR(..))
import Text.Lightyear.Position (TextPos)
import Text.Lightyear.Stream (Stream(..))

import qualified Data.Sequence as Seq


data LexElem = Lex
  { loc :: Location
  , orig :: Text
  , tok :: Token 'Sens
  }

type LexStream = Seq LexElem

mkStream :: Maybe FilePath -> [Lexeme (Result 'Sens)] -> Either ([Error], LexStream) LexStream
mkStream file xs0 = go [] Seq.empty xs0
  where
  go errs acc [] = case errs of
    [] -> Right acc
    _ -> Left (reverse errs, acc)
  go errs acc (L{loc,orig,payload}:xs) = case payload of
    Ok tok ->
      let x' = Lex
            { loc = Loc file loc (advance orig loc)
            , orig
            , tok
            }
        in go errs (acc |> x') xs
    Ignore _ -> go errs acc xs
    Error err -> go (err:errs) acc xs


instance Stream LexStream where
  type Chr LexStream = LexElem
  type Pos LexStream = TextPos
 
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

  startPosition _ = error "there is no default start location for a token stream"

-- TODO? I might want to use Location for a token position rather than just the starting TextPos
  advanceOne _ Lex{loc} _ = to loc

  advance xs loc0 = case Seq.viewr xs of
    EmptyR -> loc0
    _ :> Lex{loc} -> to loc
