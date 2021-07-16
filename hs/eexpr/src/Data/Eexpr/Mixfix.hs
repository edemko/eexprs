-- With the approach I'm taking here, mixfixes are mixfixes everywhere.
-- It would be difficult to draw a distinction between value and type mixfixes for example.
-- What I get in return for that is that this mixfix rewriting library can report error locations easily.

-- Precedence is higher when an operation is performed earlier, which means deeper into the expression tree.
-- The strangeness happens when using operator precedence, because lower-precedence operators actually get rewritten earlier.
-- That said, I will use human order in this interface: operators earlier in a list have higher precedence because they are calculated earlier.

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Eexpr.Mixfix
  ( MixfixDefinition(..)
  , Associativity(..)
  , MixfixTemplate(Nil,ConsHole,Cons)
  , MixfixTemplElem(..)
  , toTemplate
  , MixfixTemplateError(..)
  , MixfixError(..)
  , mkMixfixTable
  ) where

import Data.Eexpr.Types

import Control.Monad (forM_,when)
import Data.List (find)
import Data.List.Reverse (RList, snoc)
import Data.Maybe (fromMaybe)
import Data.Set (Set, union, intersection, difference)
import Data.Text.Short (ShortText)

import qualified Data.List.Reverse as RList
import qualified Data.Set as Set


data MixfixDefinition ann = MixfixDef
  { annotation :: !ann
  , name :: !ShortText
  , lowerPrecedenceThan :: !(Set ShortText)
  , samePrecedenceAs :: !(Set ShortText)
  , higherPrecedenceThan :: !(Set ShortText)
  , associativity :: !Associativity
  , template :: !MixfixTemplate
  }
  deriving stock (Read, Show)

data Associativity
  = LeftAssociative
  | RightAssociative
  | NonAssociative
  deriving stock (Read, Show)

newtype MixfixTemplate = Templ [MixfixTemplElem]
  deriving stock (Read, Show)
data MixfixTemplElem
  = Literal ShortText
  | Hole
  deriving stock (Read, Show)
  deriving stock (Eq)

{-# COMPLETE Nil, ConsHole, Cons #-}

pattern Nil :: MixfixTemplate
pattern Nil = Templ []

pattern ConsHole :: MixfixTemplate -> MixfixTemplate
pattern ConsHole xs <- (fromConsHole -> Just xs)
fromConsHole :: MixfixTemplate -> Maybe MixfixTemplate
fromConsHole (Templ (Hole:xs)) = Just (Templ xs)
fromConsHole _ = Nothing

pattern Cons :: ShortText -> MixfixTemplate -> MixfixTemplate
pattern Cons x xs <- (fromCons -> Just (x, xs))
fromCons :: MixfixTemplate -> Maybe (ShortText, MixfixTemplate)
fromCons (Templ (Literal x:xs)) = Just (x, Templ xs)
fromCons _ = Nothing

toTemplate :: [MixfixTemplElem] -> Either MixfixTemplateError MixfixTemplate
toTemplate xs
  | null xs = Left EmptyTemplate
  | Nothing <- find (== Hole) xs = Left ZeroHoles
  | Nothing <- find (/= Hole) xs = Left ZeroLiterals
  | otherwise = maybe (Right $ Templ xs) Left $ checkSequence True xs
  where
  checkSequence allowHole (Hole:rest) = if allowHole then checkSequence False rest else Just DoubledHoles
  checkSequence _ (Literal _ : rest) = checkSequence True rest
  checkSequence _ [] = Nothing

data MixfixTemplateError
  = EmptyTemplate
  | ZeroHoles
  | ZeroLiterals
  | DoubledHoles
  deriving stock (Read, Show)
  deriving stock (Eq)

data MixfixError ann
  = UnknownNames !(MixfixDefinition ann) (Set ShortText)
  | UnsolvablePrecedence
    { definition :: !(MixfixDefinition ann)
    , mustBeLower :: Set ShortText
    , mustBeSame :: Set ShortText
    , mustBeHigher :: Set ShortText
    }
  deriving stock (Read, Show)

mkMixfixTable :: [MixfixDefinition ann] -> ([MixfixError ann], Maybe [[MixfixDefinition ann]])
mkMixfixTable defs = extract $ unMonad (forM_ defs insert >> gets tree) (emptySt, RList.nil)
  where
  extract (_, errs, Nothing) = (RList.toList errs, Nothing)
  extract (_, errs, Just t) = (RList.toList errs, Just $ postorder t)
  postorder :: Tree ann -> [[MixfixDefinition ann]]
  postorder (Tip xs) = [RList.toList xs]
  postorder (Branch (_, l) xs (_, r)) = (postorder r) ++ (RList.toList xs) : (postorder l)


------------ Creating a Mixfix Table ------------

data MixfixState ann = St
  { knownNames :: !(Set ShortText)
  , tree :: !(Tree ann)
  }

data Tree ann
  = Tip (RList (MixfixDefinition ann))
  | Branch (Set ShortText, Tree ann) (RList (MixfixDefinition ann)) (Set ShortText, Tree ann)

emptySt :: MixfixState ann
emptySt = St
  { knownNames = Set.empty
  , tree = Tip RList.nil
  }

namesAtNode :: Tree ann -> Set ShortText
namesAtNode (Tip xs) = RList.toSet $ name <$> xs
namesAtNode (Branch _ xs _) = RList.toSet $ name <$> xs


insert :: MixfixDefinition ann -> MixfixMonad ann ()
insert def = do
  st@St{knownNames,tree} <- gets id
  let constrainingNames = lowerPrecedenceThan def `union` samePrecedenceAs def `union` higherPrecedenceThan def
      unknownNames = constrainingNames `difference` knownNames
  when (not $ Set.null unknownNames) $ do
    tell $ UnknownNames def unknownNames
  tree' <- fromMaybe tree <$> insertTree def tree
  put st{tree = tree'}
  pure ()

insertTree :: MixfixDefinition ann -> Tree ann -> MixfixMonad ann (Maybe (Tree ann))
insertTree def (Tip defs) = case surCmp def (RList.toSet $ name <$> defs) of
  -- the def is less than any definition here
  Right LT -> pure . Just $ Branch (Set.singleton $ name def, Tip (RList.nil `snoc` def)) defs (Set.empty, Tip RList.nil)
  -- the def is equal to or unconstranied by any definition here
  Right EQ -> pure . Just $ Tip (defs `snoc` def)
  -- the def is greater than any definition here
  Right GT -> pure . Just $ Branch (Set.empty, Tip RList.nil) defs (Set.singleton $ name def, Tip (RList.nil `snoc` def))
  -- inconsistency
  Left err -> tell err >> pure Nothing
insertTree def here@(Branch (l, lTree) s (r, rTree)) = case (surCmp def l, surCmp def (RList.toSet $ name <$> s), surCmp def r) of
  -- the def is less than than anything rightwards or here, so recurse into the left subtree
  (_, Right LT, Right LT) -> insertTree def lTree >>= \case
    Nothing -> pure Nothing
    Just lTree' -> pure . Just $
      Branch (Set.insert (name def) l, lTree') s (r, rTree)
  -- the def is greater than anything leftwards, less than anything rightwards, and equal-to/unconstrained-by anything here
  (Right GT, Right EQ, Right LT) -> pure . Just $
    Branch (l, lTree) (s `snoc` def) (r, rTree)
  -- the def is greater than anything leftwards or here, so recurse into the right subtree
  (Right GT, Right GT, _) -> insertTree def rTree >>= \case
    Nothing -> pure Nothing
    Just rTree' -> pure . Just $
      Branch (l, lTree) s (Set.insert (name def) r, rTree')
  -- if we get inconsistent ordering from any set that hasn't been previously handled
  -- (i.e. the inconsistencies potentially resolvable by recursion)
  -- then just report that error
  (_, Left err, _) -> tell err >> pure Nothing
  (Left err, _, _) -> tell err >> pure Nothing
  (_, _, Left err) -> tell err >> pure Nothing
  -- inconsistencies betwen the left ordering and right ordering
  -- TODO these errors are likely to be awful, but I think they should also be less common
  (Right lCmp, Right sCmp, Right rCmp) -> do
    let err = case (lCmp, sCmp, rCmp) of
                (GT, EQ, LT) -> error "unreachable"
                (GT, GT, _) -> error "unreachable"
                (_, LT, LT) -> error "unreachable"
                -- strict disorder
                (LT, _, GT) -> UnsolvablePrecedence def (namesAtNode lTree) Set.empty (namesAtNode rTree)
                (LT, GT, _) -> UnsolvablePrecedence def (namesAtNode lTree) Set.empty (namesAtNode here)
                (_, LT, GT) -> UnsolvablePrecedence def (namesAtNode here) Set.empty (namesAtNode rTree)
                -- half-strict disorder
                (LT, _, EQ) -> UnsolvablePrecedence def (namesAtNode lTree) (namesAtNode rTree) Set.empty
                (EQ, _, GT) -> UnsolvablePrecedence def Set.empty (namesAtNode lTree) (namesAtNode rTree)
                (LT, EQ, _) -> UnsolvablePrecedence def (namesAtNode lTree) (namesAtNode here) Set.empty
                (_, EQ, GT) -> UnsolvablePrecedence def Set.empty (namesAtNode here) (namesAtNode rTree)
                (EQ, GT, _) -> UnsolvablePrecedence def Set.empty (namesAtNode lTree) (namesAtNode here)
                (_, LT, EQ) -> UnsolvablePrecedence def (namesAtNode here) (namesAtNode rTree) Set.empty
                -- disjoint sets must be equal
                (EQ, _, EQ) -> UnsolvablePrecedence def Set.empty (namesAtNode lTree `union` namesAtNode rTree) Set.empty
                (EQ, EQ, _) -> UnsolvablePrecedence def Set.empty (namesAtNode lTree `union` namesAtNode here) Set.empty
                (_, EQ, EQ) -> UnsolvablePrecedence def Set.empty (namesAtNode here `union` namesAtNode rTree) Set.empty
    tell err >> pure Nothing


-- I'm using a non-standard definition of surreals.
-- Normally, a surreal is defined with left and right sets `{L|R}`, but I also allow a surreal to be defined as "the same as" another surreal.
-- Thus, we have left, same, and right sets `{L|S|R}`.
-- This should be a conservative extension, and while it is not technically needed (i.e. the "same" set is redundant since it can be computed from `L,R`),
--   the point is to allow better ergonomics and error checking when defining one definition as having the same surreal precedence as another.
--
-- A triple of sets `{L, S, R}` is a valid surreal representation when both of the following hold:
--   * `∀l∈L.∀x∈(S∪R). l < x`
--   * `∀r∈R.∀x∈(L∪S). x < r`
-- Multiple distinct representations can identify the same surreal.
--
-- Given two surreal representations, `x = {X_L|X_S|X_R}` and `y = {Y_L|Y_S|Y_R}`, we can determine an order between them.
--   * If `∀x_L∈X_L. ∀y_A∈(Y_S∪Y_R). x_L < y_A`, then `x < y`
--   * If `∀x_R∈X_R. ∀y_U∈(Y_L∪Y_S). y_U < x_R`, then `y < x`
--   * Otherwise, `x = y`
-- Note that these imply that if `x = z ∈ Y_S`, then we immediately have `x = y`.
--
-- Of course, I really should prove that these definitions really are sufficient and otherwise work the way I think they do.

-- Here, we check a possibly-valid surreal number `{L_x|S_x|R_x}` against a set of surreals `Y`.
-- Really, we are only comparing by names, so we look if any of the names in one of `L_x`, `S_x`, or `R_x` is included in the names of `Y`.
-- If exactly one of these intersections is non-empty, then the ordering is trivial.
-- If all of them are empty, then there are no constraints that would make the surreal different from the youngest surreal in `Y`;
--   i.e. I return an equal ordering.
-- If more than one is non-empty, then we must have had something in `L_x` not strictly less than something in `S_x` or `R_x`,
--   or something similar with one of `R_x` not strictly greater than something in the others.
surCmp :: MixfixDefinition ann -> Set ShortText -> Either (MixfixError ann) Ordering
surCmp def@MixfixDef{lowerPrecedenceThan,samePrecedenceAs,higherPrecedenceThan} names =
  let lt = lowerPrecedenceThan `intersection` names
      eq = samePrecedenceAs `intersection` names
      gt = higherPrecedenceThan `intersection` names
   in case (not $ Set.null lt, not $ Set.null eq, not $ Set.null gt) of
    (False, False, False) -> Right EQ
    (True, False, False) -> Right LT
    (False, True, False) -> Right EQ
    (False, False, True) -> Right GT
    _ -> Left $ UnsolvablePrecedence def lt eq gt

------ Supporting Monad ------

newtype MixfixMonad ann a = MixfixMonad
  { unMonad :: (MixfixState ann, RList (MixfixError ann)) -> (MixfixState ann, RList (MixfixError ann), Maybe a)
  }

instance Functor (MixfixMonad ann) where
  fmap f (MixfixMonad getX) = MixfixMonad $ \(st, errs) ->
    let (st', errs', mx) = getX (st, errs)
     in (st', errs', f <$> mx)

instance Applicative (MixfixMonad ann) where
  pure x = MixfixMonad $ \(st, errs) -> (st, errs, Just x)
  (MixfixMonad getF) <*> (MixfixMonad getX) = MixfixMonad $ \(st, errs) ->
    let (st', errs', mf) = getF (st, errs)
        (st'', errs'', mx) = getX (st', errs')
     in case (mf, mx) of
      (Just f, Just x) -> (st'', errs'', Just (f x))
      _ -> (st'', errs'', Nothing)

instance Monad (MixfixMonad ann) where
  (MixfixMonad getX) >>= k = MixfixMonad $ \(st, errs) ->
    case getX (st, errs) of
      (st', errs', Nothing) -> (st', errs', Nothing)
      (st', errs', Just x) -> unMonad (k x) (st', errs')

instance MonadFail (MixfixMonad ann) where
  fail _ = MixfixMonad $ \(st, errs) -> (st, errs, Nothing)

gets :: (MixfixState ann -> a) -> MixfixMonad ann a
gets f = MixfixMonad $ \(st, errs) -> (st, errs, Just (f st))

put :: MixfixState ann -> MixfixMonad ann ()
put st' = MixfixMonad $ \(_, errs) -> (st', errs, Just ())

tell :: MixfixError ann -> MixfixMonad ann ()
tell err = MixfixMonad $ \(st, errs) -> (st, errs `snoc` err, Just ())
