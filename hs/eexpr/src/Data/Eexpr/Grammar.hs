{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Data.Eexpr.Grammar
  ( Grammar
  , runGrammar
  , liftEither
  , liftMaybe
  , fromMaybe
  , Context
  , Error(..)
  , Eexpr
  -- * Combinators
  , context
  , annotation
  , map
  , mapErrors
  , ZipGrammar(..)
  , zip1
  , zip2
  , zip3
  , predicate
  -- ** Altered Alternative
  , choice
  , fail
  -- ** Primitive Grammars
  , withAnnotation
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
  , ArrowChoice(..)
  , ArrowApply(..)
  ) where

import Data.Eexpr.Grammar.Internal
import Prelude hiding (id,(.),map,zip3,fail)

import Control.Arrow (Arrow(..),ArrowChoice(..),ArrowApply(..))
import Control.Category (Category(..),(>>>),(<<<))
import Data.Eexpr.Types (Eexpr(..), Bignum(..))
import Data.List.NonEmpty ((<|))
import Data.List.NonEmpty2 (NonEmpty(..),NonEmpty2(..))
import Data.Maybe (isNothing)
import Data.Text.Short (ShortText)

import qualified Data.Eexpr.Types as Eexpr
import qualified Data.List.NonEmpty as NE


predicate :: err -> (a -> Bool) -> Grammar ann err a a
predicate err p = liftEither $ \a -> if p a then Right a else Left err

annotation :: Grammar ann err a ann
annotation = (Eexpr.annotation . NE.head) <$> context

notTheEexpr :: NonEmpty (Eexpr ann) -> err -> InnerResult ann err any
notTheEexpr ctx err = Left (Error ctx err :| [])

------------------------ Primitive Grammars ------------------------

withAnnotation :: Grammar ann err a b -> Grammar ann err a (ann, b)
withAnnotation g = annotation &&& g

symbol :: err -> Grammar ann err (Eexpr ann) ShortText
symbol err = Grammar $ \ctx e -> case e of
  Symbol _ name -> Right (e <| ctx, name)
  _ -> notTheEexpr ctx err

number :: err -> Grammar ann err (Eexpr ann) Bignum
number err = Grammar $ \ctx e -> case e of
  Number _ val -> Right (e <| ctx, val)
  _ -> notTheEexpr ctx err

integerLiteral :: err -> Grammar ann err (Eexpr ann) Bignum
integerLiteral err = number err >>> predicate err (\Bignum{fractionalExponent} -> fractionalExponent == 0)

fractionalLiteral :: err -> Grammar ann err (Eexpr ann) Bignum
fractionalLiteral err = number err >>> predicate err (\Bignum{fractionalExponent} -> fractionalExponent /= 0)

string :: err -> Grammar ann err (Eexpr ann) (ShortText, [(Eexpr ann, ShortText)])
string err = Grammar $ \ctx e -> case e of
  String _ hd tl -> Right (e <| ctx, (hd, tl))
  _ -> notTheEexpr ctx err

stringLiteral :: err -> Grammar ann err (Eexpr ann) ShortText
stringLiteral err = string err >>> second (predicate err null) >>> arr fst

paren :: err -> Grammar ann err (Eexpr ann) (Maybe (Eexpr ann))
paren err = Grammar $ \ctx e -> case e of
  Paren _ sub -> Right (e <| ctx, sub)
  _ -> notTheEexpr ctx err

parenExpr :: err -> Grammar ann err (Eexpr ann) (Eexpr ann)
parenExpr err = fromMaybe err (paren err)

nilParen :: err -> Grammar ann err (Eexpr ann) ()
nilParen err = paren err >>> predicate err isNothing >>> arr (const ())

bracket :: err -> Grammar ann err (Eexpr ann) (Maybe (Eexpr ann))
bracket err = Grammar $ \ctx e -> case e of
  Bracket _ sub -> Right (e <| ctx, sub)
  _ -> notTheEexpr ctx err

bracketExpr :: err -> Grammar ann err (Eexpr ann) (Eexpr ann)
bracketExpr err = fromMaybe err (bracket err)

nilBracket :: err -> Grammar ann err (Eexpr ann) ()
nilBracket err = bracket err >>> predicate err isNothing >>> arr (const ())

brace :: err -> Grammar ann err (Eexpr ann) (Maybe (Eexpr ann))
brace err = Grammar $ \ctx e -> case e of
  Brace _ sub -> Right (e <| ctx, sub)
  _ -> notTheEexpr ctx err

braceExpr :: err -> Grammar ann err (Eexpr ann) (Eexpr ann)
braceExpr err = fromMaybe err (brace err)

nilBrace :: err -> Grammar ann err (Eexpr ann) ()
nilBrace err = brace err >>> predicate err isNothing >>> arr (const ())

block :: err -> Grammar ann err (Eexpr ann) (NonEmpty (Eexpr ann))
block err = Grammar $ \ctx e -> case e of
  Block _ subs -> Right (e <| ctx, subs)
  _ -> notTheEexpr ctx err

predot :: err -> Grammar ann err (Eexpr ann) (Eexpr ann)
predot err = Grammar $ \ctx e -> case e of
  Predot _ sub -> Right (e <| ctx, sub)
  _ -> notTheEexpr ctx err

chain :: err -> Grammar ann err (Eexpr ann) (NonEmpty2 (Eexpr ann))
chain err = Grammar $ \ctx e -> case e of
  Chain _ subs -> Right (e <| ctx, subs)
  _ -> notTheEexpr ctx err

space :: err -> Grammar ann err (Eexpr ann) (NonEmpty2 (Eexpr ann))
space err = Grammar $ \ctx e -> case e of
  Space _ subs -> Right (e <| ctx, subs)
  _ -> notTheEexpr ctx err

ellipsis :: err -> Grammar ann err (Eexpr ann) (Maybe (Eexpr ann), Maybe (Eexpr ann))
ellipsis err = Grammar $ \ctx e -> case e of
  Ellipsis _ before after -> Right (e <| ctx, (before, after))
  _ -> notTheEexpr ctx err

colon :: err -> Grammar ann err (Eexpr ann) (Eexpr ann, Eexpr ann)
colon err = Grammar $ \ctx e -> case e of
  Colon _ before after -> Right (e <| ctx, (before, after))
  _ -> notTheEexpr ctx err

comma :: err -> Grammar ann err (Eexpr ann) [Eexpr ann]
comma err = Grammar $ \ctx e -> case e of
  Comma _ subs -> Right (e <| ctx, subs)
  _ -> notTheEexpr ctx err

semicolon :: err -> Grammar ann err (Eexpr ann) [Eexpr ann]
semicolon err = Grammar $ \ctx e -> case e of
  Semicolon _ subs -> Right (e <| ctx, subs)
  _ -> notTheEexpr ctx err




-- leadingKeyword :: ShortText -> Grammar () (Eexpr ann) (Eexpr ann)
-- leadingKeyword name = lookahead $ choice ()
--   [ space () >>> arr NE2.head >>> symbol () >>> predicate () (== name)
--   , symbol () >>> predicate () (== name)
--   ]
