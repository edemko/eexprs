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

import Prelude hiding (id,(.),map,fail)

import Data.Eexpr.Grammar

import Data.Eexpr.Mixfix (MixfixDefinition(..),Associativity(..))
import Data.Eexpr.Mixfix (MixfixTemplate(..),MixfixTemplElem(..),toTemplate,MixfixTemplateError)
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.Reverse (RList, snoc)
import Data.Text.Short (ShortText)

import qualified Data.Eexpr.Types as Eexpr
import qualified Data.List.NonEmpty as NE
import qualified Data.List.NonEmpty2 as NE2
import qualified Data.List.Reverse as RList
import qualified Data.Set as Set


recognize :: forall ann. Eexpr ann -> ([MixfixSpecError ann], Maybe (MixfixDefinition ann))
recognize expr = case runGrammar mixfixSpec expr of
  Left errs -> (NE.toList errs, Nothing)
  Right spec@Spec{ctx,name} -> go Nothing Nothing RList.nil initialDef (attrs spec)
    where
    go :: Maybe (Context ann, MixfixTemplate) -- where was the template defined
       -> Maybe (Context ann, Associativity) -- where was the associativity defined
       -> RList (MixfixSpecError ann) -- any errors/warnings that have been detected
       -> MixfixDefinition ann -- the current state of the definition
       -> [Attr ann] -- remaining attributes to process
       -> ([MixfixSpecError ann], Maybe (MixfixDefinition ann))
    go templWasHere _ errs def [] = case templWasHere of
      Just _ -> (RList.toList errs, Just def)
      Nothing -> let err = Error ctx MissingTemplate
                  in (err : RList.toList errs, Nothing)
    go templWasHere assocWasHere errs def (attr:attrs) = case attr of
      DefPrec ordering symbols ->
        let (errs', def') = goPrec ordering errs def symbols
         in go templWasHere assocWasHere errs' def' attrs
      DefAssoc assocLoc assoc -> case assocWasHere of
        Nothing ->
          go templWasHere (Just (assocLoc, assoc)) errs def{associativity = assoc} attrs
        Just (loc0, assoc0) ->
          let err = Error assocLoc (DuplicateAssociativity loc0 assoc0)
           in go templWasHere assocWasHere (errs `snoc` err) def attrs
      DefTempl templLoc templ -> case templWasHere of
        Nothing ->
          go (Just (templLoc, templ)) assocWasHere errs def{template = templ} attrs
        Just (loc0, templ0) ->
          let err = Error templLoc (DuplicateTemplate loc0 templ0)
           in go templWasHere assocWasHere (errs `snoc` err) def attrs
    initialDef = MixfixDef
      { annotation = (Eexpr.annotation . NE.head) ctx
      , name
      , lowerPrecedenceThan = Set.empty
      , samePrecedenceAs = Set.empty
      , higherPrecedenceThan = Set.empty
      , associativity = NonAssociative
      , template = Nil
      }
    -- inner loop adds symbols into the definition
    goPrec :: Ordering -- is this attribute requesting a higher/same/lower precedence?
           -> RList (Error ann (MixfixSpecErrorDesc ann)) -- initial errors
           -> MixfixDefinition ann -- the definition to update
           -> [(Context ann, ShortText)]
           -> (RList (Error ann (MixfixSpecErrorDesc ann)), MixfixDefinition ann) -- updated errors and definition
    goPrec _ errs def [] = (errs, def)
    goPrec ordering errs def ((symLoc, sym) : syms) =
      let errs' = if sym `Set.member` Set.unions [lowerPrecedenceThan def, samePrecedenceAs def, higherPrecedenceThan def]
                  then errs `snoc` Error symLoc RedundantPrecedenceConstraint
                  else errs
          def' = case ordering of
                  LT -> def{lowerPrecedenceThan = Set.insert sym (lowerPrecedenceThan def)}
                  EQ -> def{samePrecedenceAs = Set.insert sym (samePrecedenceAs def)}
                  GT -> def{higherPrecedenceThan = Set.insert sym (higherPrecedenceThan def)}
       in goPrec ordering errs' def' syms

type (a ~> b) ann = Grammar ann (MixfixSpecErrorDesc ann) a b
type MixfixSpecError ann = Error ann (MixfixSpecErrorDesc ann)
data MixfixSpecErrorDesc ann
  = NotAMixfix
  | ExpectedSpec
  | UnexpectedExprAfterDefinition
  | ExpectedNameAndBlock
  | ExpectedName
  | ExpectedBlock
  | ExpectedAttr
  | ExpectedAttrName
  | UnknownAttributeName
  | ExpectedSymbolList
  | RedundantPrecedenceConstraint
  | ExpectedAssociativity
  | DuplicateAssociativity (Context ann) Associativity
  | ExpectedSymbolsAndHoles
  | ExpectedTemplatePart
  | BadTemplate MixfixTemplateError
  | MissingTemplate
  | DuplicateTemplate (Context ann) MixfixTemplate


mixfixSpec :: (Eexpr ann ~> MixfixSpecification ann) ann
mixfixSpec = proc top -> do
  ctx <- context -< top
  (keyword, exprs) <- arr NE2.uncons <<< space NotAMixfix -< top
  _ <- predicate NotAMixfix (== "mixfix") <<< symbol NotAMixfix -< keyword
  expr <- liftMaybe UnexpectedExprAfterDefinition fromSingletonNE -< exprs
  (preName, preBlock) <- arr NE2.uncons <<< chain ExpectedNameAndBlock -< expr
  name <- symbol ExpectedName -< preName -- FIXME parse this symbol into an AttrName enum
  preAttrs <- liftMaybe UnexpectedExprAfterDefinition fromSingletonNE -< preBlock
  attrs <- map mixfixAttr <<< arr NE.toList <<< block ExpectedBlock -< preAttrs
  id -< Spec{ctx,name,attrs}
  where
  fromSingletonNE (x :| []) = Just x
  fromSingletonNE _ = Nothing

mixfixAttr :: (Eexpr ann ~> Attr ann) ann
mixfixAttr = proc preAttr -> do
  (attrName, attrBody) <- colon ExpectedAttr -< preAttr
  attrGrammar <- arr dispatch <<< symbol ExpectedAttrName -< attrName
  app -< (attrGrammar, attrBody)
  where
  dispatch "before" = proc body -> do
    arr (DefPrec GT) <<< locatedSymbolList -< body
  dispatch "simul" = proc body -> do
    arr (DefPrec EQ) <<< locatedSymbolList -< body
  dispatch "after" = proc body -> do
    arr (DefPrec LT) <<< locatedSymbolList -< body
  dispatch "assoc" = proc body -> do
    ctx <- context -< body
    assoc <- liftMaybe ExpectedAssociativity fromAssoc <<< symbol ExpectedAssociativity -< body
    arr (uncurry DefAssoc) -< (ctx, assoc)
  dispatch "pattern" = proc body -> do
    ctx <- context -< body
    templ <- mixfixTemplate <<< arr NE2.toList <<< space ExpectedSymbolsAndHoles -< body
    arr (uncurry DefTempl) -< (ctx, templ)
  dispatch _ = fail UnknownAttributeName
  fromAssoc "left" = Just LeftAssociative
  fromAssoc "right" = Just RightAssociative
  fromAssoc "none" = Just NonAssociative
  fromAssoc _ = Nothing

mixfixTemplate :: ([Eexpr ann] ~> MixfixTemplate) ann
mixfixTemplate = proc xs -> do
  parts <- map templatePart -< xs
  mapErrors BadTemplate (liftEither toTemplate) -< parts
  where
  templatePart = choice ExpectedTemplatePart
    [ arr Literal <<< symbol ()
    , arr (const Hole) <<< nilParen ()
    ]

locatedSymbolList :: (Eexpr ann ~> [(Context ann, ShortText)]) ann
locatedSymbolList = choice ExpectedSymbolList
  [ comma () >>> map inner
  , inner >>> arr (:[])
  ]
  where
  inner = context &&& symbol ()


data MixfixSpecification ann = Spec
  { ctx :: !(Context ann)
  , name :: !ShortText
  , attrs :: [Attr ann]
  }

data Attr ann
  = DefPrec Ordering [(Context ann, ShortText)]
  | DefAssoc (Context ann) Associativity
  | DefTempl (Context ann) MixfixTemplate
