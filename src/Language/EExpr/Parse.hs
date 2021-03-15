{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

module Language.EExpr.Parse
  ( Parser
  , run
  , arr, (>->)
  , Error(..)
  , fail
  -- * Generic Parsers
  , many
  , some
  , nonNothing
  , nonEmpty
  -- * Fundamental EExpr Parsers
  , here
  , symbolAtom
  , integerAtom
  , rationalAtom
  , stringAtom
  , templateString
  , paren
  , bracket
  , brace
  , indent
  , apply
  , ellipsis
  , colon
  , commas
  , semicolons
  , chain
  , prefixDot
  ) where

import Prelude hiding (fail)

import Language.EExpr.Parse.Internal
import Language.EExpr.Types hiding (Error)

import Data.Either (partitionEithers)
import Data.Semigroup (sconcat)
import Data.Text (Text)

------------------ Generic ------------------

many :: Parser a b -> Parser [a] [b]
many (P p) = P $ \loc ys -> combine loc $ map (p loc) ys
  where
  combine loc0 results = case partitionEithers results of
    ([], ys) -> Right (loc0, snd <$> ys)
    (err:errs, _) -> Left $ sconcat (err:|errs)

some :: String -> Parser a b -> Parser [a] [b]
some msg (P p) = P $ \loc -> \case
  [] -> Left $ Error loc msg :| []
  xs -> unP (many (P p)) loc xs

nonNothing :: String -> Parser (Maybe a) a
nonNothing msg = arr $ maybe (Left msg) Right

nonEmpty :: String -> Parser [a] (NonEmpty a)
nonEmpty msg = arr $ \case
  [] -> Left msg
  (x:xs) -> Right (x:|xs)


------------------ EExpr-related ------------------

here :: Parser EExpr Location
here = P $ \_ -> \case
  Atom loc _ -> Right (loc, loc)
  Combine loc _ _ -> Right (loc, loc)

symbolAtom :: Parser EExpr Text
symbolAtom = P $ \_ -> \case
  Atom loc (SymAtom x) -> Right (loc, x)
  other -> Left $ Error (location other) "expected symbol" :| []

integerAtom :: Parser EExpr (Text, Integer)
integerAtom = P $ \_ -> \case
  Atom loc (IntAtom orig i) -> Right (loc, (orig, i))
  other -> Left $ Error (location other) "expected integer" :| []

rationalAtom :: Parser EExpr (Text, Rational)
rationalAtom = P $ \_ -> \case
  Atom loc (RatAtom orig x) -> Right (loc, (orig, x))
  other -> Left $ Error (location other) "expected rational" :| []

-- TODO number atom

stringAtom :: Parser EExpr Text
stringAtom = P $ \_ -> \case
  StrAtom loc str -> Right (loc, str)
  other -> Left $ Error (location other) "expected string" :| []


templateString :: Parser EExpr (Text, [(EExpr, Text)])
templateString = P $ \_ -> \case
  Combine loc StrTempl parts -> Right (loc, parts)
  other -> Left $ Error (location other) "expected string template" :| []

paren :: Parser EExpr (Maybe EExpr)
paren = P $ \_ -> \case
  Combine loc Paren inner -> Right (loc, inner)
  other -> Left $ Error (location other) "expected parenthesis" :| []

bracket :: Parser EExpr (Maybe EExpr)
bracket = P $ \_ -> \case
  Combine loc Bracket inner -> Right (loc, inner)
  other -> Left $ Error (location other) "expected square brackets" :| []

brace :: Parser EExpr (Maybe EExpr)
brace = P $ \_ -> \case
  Combine loc Brace inner -> Right (loc, inner)
  other -> Left $ Error (location other) "expected curly braces" :| []

indent :: Parser EExpr (NonEmpty EExpr)
indent = P $ \_ -> \case
  Combine loc Indent inner -> Right (loc, inner)
  other -> Left $ Error (location other) "expected indented block" :| []

apply :: Parser EExpr (NonEmpty EExpr)
apply = P $ \_ -> \case
  Combine loc Indent xs -> Right (loc, xs)
  other -> Left $ Error (location other) "expected application" :| []

ellipsis :: Parser EExpr (Maybe EExpr, Maybe EExpr)
ellipsis = P $ \_ -> \case
  Combine loc Ellipsis xy -> Right (loc, xy)
  other -> Left $ Error (location other) "expected ellipsis" :| []

colon :: Parser EExpr (EExpr, EExpr)
colon = P $ \_ -> \case
  Combine loc Colon xy -> Right (loc, xy)
  other -> Left $ Error (location other) "expected colon" :| []

commas :: Parser EExpr [EExpr]
commas = P $ \_ -> \case
  Combine loc Comma es -> Right (loc, es)
  other -> Left $ Error (location other) "expected commas" :| []

semicolons :: Parser EExpr [EExpr]
semicolons = P $ \_ -> \case
  Combine loc Semicolon es -> Right (loc, es)
  other -> Left $ Error (location other) "expected semicolons" :| []

chain :: Parser EExpr (NonEmpty2 EExpr)
chain = P $ \_ -> \case
  Combine loc Chain es -> Right (loc, es)
  other -> Left $ Error (location other) "expected chain" :| []

prefixDot :: Parser EExpr EExpr
prefixDot = P $ \_ -> \case
  Combine loc SyntheticDot e' -> Right (loc, e')
  other -> Left $ Error (location other) "expected prefix dot" :| []
