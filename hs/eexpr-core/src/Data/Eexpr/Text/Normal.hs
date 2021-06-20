{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Eexpr.Text.Normal
  ( normalText
  ) where

import Data.Eexpr.Types
import Prelude hiding (significand)

import Data.Char (ord,isPrint)
import Data.List.NonEmpty2 (NonEmpty2(..),NonEmpty(..))
import Data.Text (Text)
import Data.Text.Short (ShortText)
import Numeric (showIntAtBase,showOct,showHex)

import qualified Data.List.NonEmpty as NE
import qualified Data.List.NonEmpty2 as NE2
import qualified Data.Text as T
import qualified Data.Text.Short as ST

-- | The idea here is to generate a string that will parse back into the given eexpr,
--   but also have exactly one such string for each eexpr.
normalText :: Eexpr ann -> Text
normalText = norm "\n"

norm :: Text -> Eexpr a -> Text
norm _ (Symbol _ name) = ST.toText name
norm _ (Number _ value) = normValue value
norm ind (String _ str rest) = mconcat
  [ "\""
  , normString str
  , T.concat $ (\(e, s) -> "`" <> norm ind e <> "`" <> normString s) <$> rest
  , "\""
  ]
norm ind (Paren _ sub) = normWrap ind "(" ")" sub
norm ind (Bracket _ sub) = normWrap ind "[" "]" sub
norm ind (Brace _ sub) = normWrap ind "{" "}" sub
norm ind (Block _ subs) = ":" <> normBlock ind subs
norm ind (Predot _ sub) = "." <> norm ind sub
norm ind (Chain _ subs) = normChain ind subs
norm ind (Space _ subs) = T.intercalate " " $ norm ind <$> (NE2.toList subs)
norm ind (Ellipsis _ before after) = maybe "" (norm ind) before <> ".." <> maybe "" (norm ind) after
norm ind (Colon _ before after) = norm ind before <> ": " <> norm ind after
norm ind (Comma _ subs) = T.intercalate ", " $ norm ind <$> (NE2.toList subs)
norm ind (Semicolon _ subs) = T.intercalate "; " $ norm ind <$> (NE2.toList subs)



normValue :: Bignum -> Text
normValue Bignum{significand,radix,fractionalExponent,explicitExponent} =
  let (radixMark, radixShow) = case radix of
        Base2 -> ("0b", flip showBin "")
        Base8 -> ("0o", flip showOct "")
        Base10 -> ("", show)
        Base12 -> ("0z", flip showDoz "")
        Base16 -> ("0x", flip showHex "")
      sig = radixShow significand
      (mantissa, fractional) = splitAt (length sig - fromIntegral fractionalExponent) sig
   in mconcat
    [ radixMark
    , T.pack mantissa
    , if fractionalExponent == 0 then "" else T.pack ('.':fractional)
    , if explicitExponent == 0 then "" else T.pack ('^':show explicitExponent)
    ]
  where
  showBin = showIntAtBase 2 (['0','1'] !!)
  showDoz = showIntAtBase 12 (['0','1','2','3','4','5','6','7','8','9','↊','↋'] !!)

normString :: ShortText -> Text
normString = T.concatMap go . ST.toText
  where
  go c
    | isPrint c = T.singleton c
    | ord c <= 0xFF = "\\x" <> showAligned 2 c
    | ord c <= 0xFFFF = "\\u" <> showAligned 4 c
    | otherwise = "\\U" <> showAligned 6 c
  showAligned n c =
    let short = showHex (ord c) ""
        pad = replicate (n - length short) '0'
     in T.pack $ pad <> short

normWrap :: Text -> Text -> Text -> (Maybe (Eexpr a)) -> Text
normWrap _ open close Nothing = open <> close
normWrap ind open close (Just (Block _ subs)) = mconcat
  [ open, normBlock ind subs, ind, close ]
normWrap ind open close (Just sub) = open <> norm ind sub <> close

normBlock :: Text -> NonEmpty (Eexpr a) -> Text
normBlock ind (NE.toList -> subs) = mconcat $ line <$> subs
  where
  ind' = ind <> "  "
  line sub = ind' <> norm ind' sub

normChain :: Text -> NonEmpty2 (Eexpr a) -> Text
normChain ind subs = mconcat $ addDots subs
  where
  addDots (x:|| y :| xs) = norm ind x : (go <$> y:xs)
  go x@(String _ _ _) = norm ind x
  go x@(Paren _ _) = norm ind x
  go x@(Bracket _ _) = norm ind x
  go x@(Brace _ _) = norm ind x
  go x@(Block _ _) = norm ind x
  go x = "." <> norm ind x
