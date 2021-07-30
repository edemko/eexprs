{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Data.Eexpr.Mixfix.Grammar
  ( recognize
  , MixfixSpecError
  , MixfixSpecErrorDesc(..)
  ) where

import Prelude hiding (id,(.),map,zip,fail)

import Data.Eexpr.Grammar

import Data.Eexpr.Mixfix (Definition(..),Associativity(..),Template(..),TemplElem(..),toTemplate)
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.Reverse (RList, snoc)
import Data.Text.Short (ShortText)

import qualified Data.Eexpr.Mixfix as Mixfix
import qualified Data.Eexpr.Types as Eexpr
import qualified Data.List.NonEmpty as NE
import qualified Data.List.NonEmpty2 as NE2
import qualified Data.List.Reverse as RList
import qualified Data.Set as Set


recognize :: Eexpr ann -> ([MixfixSpecError ann], Maybe (Mixfix.Definition ann))
recognize expr = case runGrammar mixfixSpec expr of
  Left errs -> (NE.toList errs, Nothing)
  Right spec -> specToDef spec

type (a ~> b) ann = Grammar ann (MixfixSpecErrorDesc ann) a b
type MixfixSpecError ann = Error ann (MixfixSpecErrorDesc ann)
data MixfixSpecErrorDesc ann
  = NotAMixfix
  | UnexpectedExprAfterDefinition (Eexpr ann)
  | ExpectedName
  | ExpectedAttrsBlock
  | ExpectedAttr
  | ExpectedAttrName
  | UnknownAttributeName
  | ExpectedSymbolList
  | RedundantPrecedenceConstraint
  | ExpectedAssociativity
  | DuplicateAssociativity (Context ann) Associativity
  | ExpectedSymbolsAndHoles
  | ExpectedTemplatePart
  | BadTemplate Mixfix.TemplateError
  | MissingTemplate
  | DuplicateTemplate (Context ann) Mixfix.Template
  deriving (Show)

-- | Detects a space-separated list of eexprs that starts with a given keyword.
-- This is the closest analogue of special forms in Lisp.
specialFormSpace :: MixfixSpecErrorDesc ann -- ^ error when special form is not detected
                 -> ShortText -- ^ keyword that introduces the special form
                 -> (Eexpr ann ~> NonEmpty (Eexpr ann)) ann
specialFormSpace err keyword = proc expr -> do
  (lead, exprs) <- arr NE2.uncons <<< space err -< expr
  _ <- (predicate err (== keyword) <<< symbol err) -< lead
  id -< exprs

mixfixSpec :: (Eexpr ann ~> MixfixSpecification ann) ann
mixfixSpec = do
  ctx <- context
  (name, attrs) <-  specialFormSpace NotAMixfix "mixfix"
                >>> arr NE.toList
                >>> zip2 ( ZG mixfixName ExpectedName
                         , ZG mixfixAttrs ExpectedAttrsBlock
                         ) UnexpectedExprAfterDefinition
  pure Spec{ctx,name,attrs}

mixfixName :: (Eexpr ann ~> ShortText) ann
mixfixName = symbol ExpectedName

mixfixAttrs :: (Eexpr ann ~> [Attr ann]) ann
mixfixAttrs = block ExpectedAttrsBlock >>> map mixfixAttr

mixfixAttr :: (Eexpr ann ~> Attr ann) ann
mixfixAttr = proc preAttr -> do
  (attrName, attrBody) <- (symbol ExpectedAttrName)***id <<< colon ExpectedAttr -< preAttr
  case attrName of
    "before" -> DefPrec GT <$> locatedSymbols -< attrBody
    "simul" -> DefPrec EQ <$> locatedSymbols -< attrBody
    "after" -> DefPrec LT <$> locatedSymbols -< attrBody
    "assoc" -> DefAssoc <$> context <*> mixfixAssociativity -< attrBody
    "pattern" -> DefTempl <$> context <*> mixfixTemplate -< attrBody
    _ -> (| (fail UnknownAttributeName) |)

locatedSymbols :: (Eexpr ann ~> [(Context ann, ShortText)]) ann
locatedSymbols = choice ExpectedSymbolList
  [ comma () >>> map locatedSymbol
  , (:[]) <$> locatedSymbol
  ]
  where
  locatedSymbol = context &&& symbol ()

mixfixAssociativity :: (Eexpr ann ~> Associativity) ann
mixfixAssociativity = proc it -> do
  name <- symbol ExpectedAssociativity -< it
  case name of
    "left" -> (| (pure LeftAssociative) |)
    "right" -> (| (pure RightAssociative) |)
    "none" -> (| (pure NonAssociative) |)
    _ -> (| (fail ExpectedAssociativity) |)

mixfixTemplate :: (Eexpr ann ~> Mixfix.Template) ann
mixfixTemplate
  =   space ExpectedSymbolsAndHoles
  >>> arr NE2.toList
  >>> map templatePart
  >>> mapErrors BadTemplate (liftEither toTemplate)
  where
  templatePart = choice ExpectedTemplatePart
    [ Literal <$> symbol ()
    , Hole <$ nilParen ()
    ]


data MixfixSpecification ann = Spec
  { ctx :: !(Context ann)
  , name :: !ShortText
  , attrs :: [Attr ann]
  }

data Attr ann
  = DefPrec Ordering [(Context ann, ShortText)]
  | DefAssoc (Context ann) Associativity
  | DefTempl (Context ann) Mixfix.Template

-- | This aggregates the attributes of a specification into a sensible mixfix definition.
-- The underlying algorithm is a loop over the attributes to ensure that everything is defined a correct number of times.
-- Additionally, it checks that none of the precedence specifications are redundant or inconsistent.
--
--   * zero or one associativity
--   * exactly one template
--   * each other operator is listed for precedence at most once
--
-- If the mixfix has inconsistent precedence relations, it is not a fatal error yet.
-- Instead, tabulation for any set of mixfixes will fail when this definition is included in the set.
-- This allows other precedence tables to be computed before requiring the inconsistency t obe reported.
specToDef :: forall ann. MixfixSpecification ann -> ([MixfixSpecError ann], Maybe (Mixfix.Definition ann))
specToDef Spec{ctx,name,attrs} = mergeAttrs Nothing Nothing RList.nil initialDef attrs
  where
  initialDef :: Mixfix.Definition ann
  initialDef = MixfixDef
    { annotation = (Eexpr.annotation . NE.head) ctx
    , name
    , lowerPrecedenceThan = Set.empty
    , samePrecedenceAs = Set.empty
    , higherPrecedenceThan = Set.empty
    , associativity = NonAssociative
    , template = Nil
    }
  mergeAttrs :: Maybe (Context ann, Mixfix.Template) -- where was the template defined
     -> Maybe (Context ann, Associativity) -- where was the associativity defined
     -> RList (MixfixSpecError ann) -- any errors/warnings that have been detected
     -> Mixfix.Definition ann -- the current state of the definition
     -> [Attr ann] -- remaining attributes to process
     -> ([MixfixSpecError ann], Maybe (Mixfix.Definition ann))
  mergeAttrs templWasHere assocWasHere errs def = \case
    DefPrec ordering symbols : rest ->
      let (errs', def') = mergePrecedence ordering errs def symbols
       in mergeAttrs templWasHere assocWasHere errs' def' rest
    DefAssoc assocLoc assoc : rest -> case assocWasHere of
      Nothing ->
        mergeAttrs templWasHere (Just (assocLoc, assoc)) errs def{associativity = assoc} rest
      Just (loc0, assoc0) ->
        let err = Error assocLoc (DuplicateAssociativity loc0 assoc0)
         in mergeAttrs templWasHere assocWasHere (errs `snoc` err) def rest
    DefTempl templLoc templ : rest -> case templWasHere of
      Nothing ->
        mergeAttrs (Just (templLoc, templ)) assocWasHere errs def{template = templ} rest
      Just (loc0, templ0) ->
        let err = Error templLoc (DuplicateTemplate loc0 templ0)
         in mergeAttrs templWasHere assocWasHere (errs `snoc` err) def rest
    [] -> case templWasHere of
      Just _ -> (RList.toList errs, Just def)
      Nothing -> let err = Error ctx MissingTemplate
                  in (err : RList.toList errs, Nothing)
  -- inner loop adds symbols into the definition
  mergePrecedence ::
       Ordering -- is this attribute requesting a higher/same/lower precedence?
    -> RList (Error ann (MixfixSpecErrorDesc ann)) -- initial errors
    -> Mixfix.Definition ann -- the definition to update
    -> [(Context ann, ShortText)]
    -> (RList (Error ann (MixfixSpecErrorDesc ann)), Mixfix.Definition ann) -- updated errors and definition
  mergePrecedence ordering errs def = \case
    (symLoc, sym) : syms ->
      let errs' = if sym `Set.member` Set.unions [lowerPrecedenceThan def, samePrecedenceAs def, higherPrecedenceThan def]
                  then errs `snoc` Error symLoc RedundantPrecedenceConstraint
                  else errs
          def' = case ordering of
                  LT -> def{lowerPrecedenceThan = Set.insert sym (lowerPrecedenceThan def)}
                  EQ -> def{samePrecedenceAs = Set.insert sym (samePrecedenceAs def)}
                  GT -> def{higherPrecedenceThan = Set.insert sym (higherPrecedenceThan def)}
       in mergePrecedence ordering errs' def' syms
    [] -> (errs, def)
