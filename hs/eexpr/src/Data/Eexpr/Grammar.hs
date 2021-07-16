{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Data.Eexpr.Grammar
  ( Grammar
  , runGrammar
  , liftEither
  , liftMaybe
  , fromMaybe
  , GrammarError(..)
  , Eexpr
  , Location(..)
  -- * Combinators
  , location
  , map
  , mapErrors
  , predicate
  -- ** Altered Alternative
  , choice
  , fail
  -- ** Primitive Grammars
  , withLocation
  , symbol
  , number
  , string
  , paren
  , bracket
  , brace
  , block
  , predot
  , chain
  , space
  , ellipsis
  , colon
  , comma
  , semicolon
  -- ** Common Idioms
  , integerLiteral
  , fractionalLiteral
  , stringLiteral
  , parenExpr
  , bracketExpr
  , braceExpr
  , nilParen
  , nilBracket
  , nilBrace
  -- ** Re-exports
  , Applicative(..)
  , Category(..)
  ,(>>>)
  ,(<<<)
  , Arrow(..)
  , ArrowApply(..)
  ) where

import Data.Eexpr.Grammar.Internal
import Prelude hiding (id,map,fail)

import Data.Maybe (isNothing)
import Control.Arrow (Arrow(..),ArrowApply(..))
import Control.Category (Category(..),(>>>),(<<<))
import Data.Eexpr.Types (Eexpr(..), annotation, Location(..), Bignum(..))
import Data.Text.Short (ShortText)
import Data.List.NonEmpty2 (NonEmpty(..),NonEmpty2(..))


predicate :: err -> (a -> Bool) -> Grammar err a a
predicate err p = liftEither $ \a -> if p a then Right a else Left err



------------------------ Primitive Grammars ------------------------

withLocation :: Grammar err a b -> Grammar err a (Location, b)
withLocation g = location &&& g

notTheEexpr :: err -> Eexpr Location -> Either (NonEmpty (GrammarError err)) a
notTheEexpr err e = Left $ GrammarError (annotation e) err :| []

symbol :: err -> Grammar err (Eexpr Location) ShortText
symbol err = Grammar $ const $ \case
  Symbol l' name -> Right (l', name)
  x -> notTheEexpr err x

number :: err -> Grammar err (Eexpr Location) Bignum
number err = Grammar $ const $ \case
  Number l' val -> Right (l', val)
  x -> notTheEexpr err x

integerLiteral :: err -> Grammar err (Eexpr Location) Bignum
integerLiteral err = number err >>> predicate err (\Bignum{fractionalExponent} -> fractionalExponent == 0)

fractionalLiteral :: err -> Grammar err (Eexpr Location) Bignum
fractionalLiteral err = number err >>> predicate err (\Bignum{fractionalExponent} -> fractionalExponent /= 0)

string :: err -> Grammar err (Eexpr Location) (ShortText, [(Eexpr Location, ShortText)])
string err = Grammar $ const $ \case
  String l' hd tl -> Right (l', (hd, tl))
  x -> notTheEexpr err x

stringLiteral :: err -> Grammar err (Eexpr Location) ShortText
stringLiteral err = string err >>> second (predicate err null) >>> arr fst

paren :: err -> Grammar err (Eexpr Location) (Maybe (Eexpr Location))
paren err = Grammar $ const $ \case
  Paren l' sub -> Right (l', sub)
  x -> notTheEexpr err x

parenExpr :: err -> Grammar err (Eexpr Location) (Eexpr Location)
parenExpr err = fromMaybe err (paren err)

nilParen :: err -> Grammar err (Eexpr Location) ()
nilParen err = paren err >>> predicate err isNothing >>> arr (const ())

bracket :: err -> Grammar err (Eexpr Location) (Maybe (Eexpr Location))
bracket err = Grammar $ const $ \case
  Bracket l' sub -> Right (l', sub)
  x -> notTheEexpr err x

bracketExpr :: err -> Grammar err (Eexpr Location) (Eexpr Location)
bracketExpr err = fromMaybe err (bracket err)

nilBracket :: err -> Grammar err (Eexpr Location) ()
nilBracket err = bracket err >>> predicate err isNothing >>> arr (const ())

brace :: err -> Grammar err (Eexpr Location) (Maybe (Eexpr Location))
brace err = Grammar $ const $ \case
  Brace l' sub -> Right (l', sub)
  x -> notTheEexpr err x

braceExpr :: err -> Grammar err (Eexpr Location) (Eexpr Location)
braceExpr err = fromMaybe err (brace err)

nilBrace :: err -> Grammar err (Eexpr Location) ()
nilBrace err = brace err >>> predicate err isNothing >>> arr (const ())

block :: err -> Grammar err (Eexpr Location) (NonEmpty (Eexpr Location))
block err = Grammar $ const $ \case
  Block l' subs -> Right (l', subs)
  x -> notTheEexpr err x

predot :: err -> Grammar err (Eexpr Location) (Eexpr Location)
predot err = Grammar $ const $ \case
  Predot l' sub -> Right (l', sub)
  x -> notTheEexpr err x

chain :: err -> Grammar err (Eexpr Location) (NonEmpty2 (Eexpr Location))
chain err = Grammar $ const $ \case
  Chain l' subs -> Right (l', subs)
  x -> notTheEexpr err x

space :: err -> Grammar err (Eexpr Location) (NonEmpty2 (Eexpr Location))
space err = Grammar $ const $ \case
  Space l' subs -> Right (l', subs)
  x -> notTheEexpr err x

ellipsis :: err -> Grammar err (Eexpr Location) (Maybe (Eexpr Location), Maybe (Eexpr Location))
ellipsis err = Grammar $ const $ \case
  Ellipsis l' before after -> Right (l', (before, after))
  x -> notTheEexpr err x

colon :: err -> Grammar err (Eexpr Location) (Eexpr Location, Eexpr Location)
colon err = Grammar $ const $ \case
  Colon l' before after -> Right (l', (before, after))
  x -> notTheEexpr err x

comma :: err -> Grammar err (Eexpr Location) [Eexpr Location]
comma err = Grammar $ const $ \case
  Comma l' subs -> Right (l', subs)
  x -> notTheEexpr err x

semicolon :: err -> Grammar err (Eexpr Location) [Eexpr Location]
semicolon err = Grammar $ const $ \case
  Semicolon l' subs -> Right (l', subs)
  x -> notTheEexpr err x




-- leadingKeyword :: ShortText -> Grammar () (Eexpr Location) (Eexpr Location)
-- leadingKeyword name = lookahead $ choice ()
--   [ space () >>> arr NE2.head >>> symbol () >>> predicate () (== name)
--   , symbol () >>> predicate () (== name)
--   ]
