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
  ( -- * Mixfix Definitions
    Definition(..)
  , mapAnnotation
  , Associativity(..)
  , Template(Nil,ConsHole,Cons)
  , TemplElem(..)
  , toTemplate
  , TemplateError(..)
  , Error(..)
  -- * Mixfix Tables
  , tabulate
  , Table(..)
  ) where

import Control.Monad (forM_,when)
import Data.List (find)
import Data.List.Reverse (RList, snoc)
import Data.Set (Set, union, intersection, difference)
import Data.Text.Short (ShortText)

import qualified Data.List.Reverse as RList
import qualified Data.Set as Set


data Definition ann = MixfixDef
  { annotation :: !ann
  -- TODO context (which is just a string), or perhaps tags (just a list of strings)
  , name :: !ShortText
  , lowerPrecedenceThan :: !(Set ShortText)
  , samePrecedenceAs :: !(Set ShortText)
  , higherPrecedenceThan :: !(Set ShortText)
  , associativity :: !Associativity
  , template :: !Template
  }
  deriving stock (Read, Show)
  deriving stock (Eq)

mapAnnotation :: (a -> b) -> Definition a -> Definition b
mapAnnotation f d = d{annotation = f (annotation d) }

data Associativity
  = LeftAssociative
  | RightAssociative
  | NonAssociative
  deriving stock (Read, Show)
  deriving stock (Eq)

newtype Template = Templ [TemplElem]
  deriving stock (Read, Show)
  deriving stock (Eq)
data TemplElem
  = Literal ShortText
  | Hole
  deriving stock (Read, Show)
  deriving stock (Eq)

{-# COMPLETE Nil, ConsHole, Cons #-}

pattern Nil :: Template
pattern Nil = Templ []

pattern ConsHole :: Template -> Template
pattern ConsHole xs <- (fromConsHole -> Just xs)
fromConsHole :: Template -> Maybe Template
fromConsHole (Templ (Hole:xs)) = Just (Templ xs)
fromConsHole _ = Nothing

pattern Cons :: ShortText -> Template -> Template
pattern Cons x xs <- (fromCons -> Just (x, xs))
fromCons :: Template -> Maybe (ShortText, Template)
fromCons (Templ (Literal x:xs)) = Just (x, Templ xs)
fromCons _ = Nothing

toTemplate :: [TemplElem] -> Either TemplateError Template
toTemplate xs
  | null xs = Left EmptyTemplate
  | Nothing <- find (== Hole) xs = Left ZeroHoles
  | Nothing <- find (/= Hole) xs = Left ZeroLiterals
  | otherwise = maybe (Right $ Templ xs) Left $ checkSequence True xs
  where
  checkSequence allowHole (Hole:rest) = if allowHole then checkSequence False rest else Just DoubledHoles
  checkSequence _ (Literal _ : rest) = checkSequence True rest
  checkSequence _ [] = Nothing

data TemplateError
  = EmptyTemplate
  | ZeroHoles
  | ZeroLiterals
  | DoubledHoles
  deriving stock (Read, Show)
  deriving stock (Eq)

data Error ann
  = UnknownNames !(Definition ann) (Set ShortText)
  | UnsolvablePrecedence
    { definition :: !(Definition ann)
    , mustBeLower :: Set ShortText
    , mustBeSame :: Set ShortText
    , mustBeHigher :: Set ShortText
    }
  deriving stock (Read, Show)


------------ Creating a Mixfix Table ------------

newtype Table ann = Table { rows :: [[Definition ann]] }
  deriving stock (Read, Show)
  deriving stock (Eq)

-- result is a table in ascending order of precedence; order within each precedence level is the order of definition
tabulate :: [Definition ann] -> ([Error ann], Maybe (Table ann))
tabulate defs = extract $ unMonad (forM_ defs insert >> gets tree) (emptySt, RList.nil)
  where
  extract (_, errs, Nothing) = (RList.toList errs, Nothing)
  extract (_, errs, Just t) = (RList.toList errs, Just . Table $ preorder t)
  preorder :: Tree ann -> [[Definition ann]]
  preorder (Tip xs) = if RList.null xs then [] else [RList.toList xs]
  preorder (Branch (_, l) xs (_, r))
    =  preorder l
    ++ (if RList.null xs then [] else [RList.toList xs])
    ++ preorder r

data MixfixState ann = St
  { knownNames :: !(Set ShortText)
  , tree :: !(Tree ann)
  }

data Tree ann
  = Tip (RList (Definition ann))
  | Branch (Set ShortText, Tree ann) (RList (Definition ann)) (Set ShortText, Tree ann)

emptySt :: MixfixState ann
emptySt = St
  { knownNames = Set.empty
  , tree = Tip RList.nil
  }

namesAtNode :: Tree ann -> Set ShortText
namesAtNode (Tip xs) = RList.toSet $ name <$> xs
namesAtNode (Branch _ xs _) = RList.toSet $ name <$> xs


insert :: Definition ann -> MixfixMonad ann ()
insert def = do
  St{knownNames,tree} <- gets id
  let constrainingNames = lowerPrecedenceThan def `union` samePrecedenceAs def `union` higherPrecedenceThan def
      unknownNames = constrainingNames `difference` knownNames
  when (not $ Set.null unknownNames) $ do
    tell $ UnknownNames def unknownNames
  insertTree def tree >>= \case
    Nothing -> pure ()
    Just tree' -> put St
      { knownNames = Set.insert (name def) knownNames
      , tree = tree'
      }

insertTree :: Definition ann -> Tree ann -> MixfixMonad ann (Maybe (Tree ann))
insertTree def (Tip defs) = case surCmp def (RList.toSet $ name <$> defs) of
  -- the def is less than any definition here
  Right (Just LT) -> goLeft
  -- the def is equal to or unconstranied by any definition here
  Right (Just EQ) -> goHere
  Right Nothing -> goHere
  -- the def is greater than any definition here
  Right (Just GT) -> goRight
  -- inconsistency
  Left err -> tell err >> pure Nothing
  where
  goLeft = pure . Just $ Branch (Set.singleton $ name def, Tip (RList.nil `snoc` def)) defs (Set.empty, Tip RList.nil)
  goHere = pure . Just $ Tip (defs `snoc` def)
  goRight = pure . Just $ Branch (Set.empty, Tip RList.nil) defs (Set.singleton $ name def, Tip (RList.nil `snoc` def))
insertTree def here@(Branch (l, lTree) s (r, rTree)) = case (surCmp def l, surCmp def (RList.toSet $ name <$> s), surCmp def r) of
  -- the def is greater than anything leftwards, less than anything rightwards, and equal-to/unconstrained-by anything here
  (Right (Just GT), Right (Just EQ), Right (Just LT)) -> goHere
  (Right (Just GT), Right (Just EQ), Right Nothing) -> goHere
  (Right (Just GT), Right Nothing, Right (Just LT)) -> goHere
  (Right (Just GT), Right Nothing, Right Nothing) -> goHere
  (Right Nothing, Right (Just EQ), Right (Just LT)) -> goHere
  (Right Nothing, Right (Just EQ), Right Nothing) -> goHere
  (Right Nothing, Right Nothing, Right (Just LT)) -> goHere
  (Right Nothing, Right Nothing, Right Nothing) -> goHere
  -- the def is less than than anything rightwards or here, so recurse into the left subtree
  (_, Right (Just LT), Right (Just LT)) -> goLeft
  (_, Right (Just LT), Right Nothing) -> goLeft
  (_, Right Nothing, Right (Just LT)) -> goLeft
  (_, Right Nothing, Right Nothing) -> goLeft
  -- the def is greater than anything leftwards or here, so recurse into the right subtree
  (Right (Just GT), Right (Just GT), _) -> goRight
  (Right (Just GT), Right Nothing, _) -> goRight
  (Right Nothing, Right (Just GT), _) -> goRight
  (Right Nothing, Right Nothing, _) -> goRight
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
                (Just GT, Just EQ, Just LT) -> error "unreachable"
                (Just GT, Just EQ, Nothing) -> error "unreachable"
                (Just GT, Nothing, Just LT) -> error "unreachable"
                (Just GT, Nothing, Nothing) -> error "unreachable"
                (Nothing, Just EQ, Just LT) -> error "unreachable"
                (Nothing, Just EQ, Nothing) -> error "unreachable"
                (Nothing, Nothing, Just LT) -> error "unreachable"
                (Nothing, Nothing, Nothing) -> error "unreachable"
                (_, Just LT, Just LT) -> error "unreachable"
                (_, Just LT, Nothing) -> error "unreachable"
                (_, Nothing, Just LT) -> error "unreachable"
                (_, Nothing, Nothing) -> error "unreachable"
                (Just GT, Just GT, _) -> error "unreachable"
                (Just GT, Nothing, _) -> error "unreachable"
                (Nothing, Just GT, _) -> error "unreachable"
                (Nothing, Nothing, _) -> error "unreachable"
                -- strict disorder
                (Just LT, _, Just GT) -> UnsolvablePrecedence def (namesAtNode lTree) Set.empty (namesAtNode rTree)
                (Just LT, Just GT, _) -> UnsolvablePrecedence def (namesAtNode lTree) Set.empty (namesAtNode here)
                (_, Just LT, Just GT) -> UnsolvablePrecedence def (namesAtNode here) Set.empty (namesAtNode rTree)
                -- half-strict disorder
                (Just LT, _, Just EQ) -> UnsolvablePrecedence def (namesAtNode lTree) (namesAtNode rTree) Set.empty
                (Just EQ, _, Just GT) -> UnsolvablePrecedence def Set.empty (namesAtNode lTree) (namesAtNode rTree)
                (Just LT, Just EQ, _) -> UnsolvablePrecedence def (namesAtNode lTree) (namesAtNode here) Set.empty
                (_, Just EQ, Just GT) -> UnsolvablePrecedence def Set.empty (namesAtNode here) (namesAtNode rTree)
                (Just EQ, Just GT, _) -> UnsolvablePrecedence def Set.empty (namesAtNode lTree) (namesAtNode here)
                (_, Just LT, Just EQ) -> UnsolvablePrecedence def (namesAtNode here) (namesAtNode rTree) Set.empty
                -- disjoint sets must be equal
                (Just EQ, _, Just EQ) -> UnsolvablePrecedence def Set.empty (namesAtNode lTree `union` namesAtNode rTree) Set.empty
                (Just EQ, Just EQ, _) -> UnsolvablePrecedence def Set.empty (namesAtNode lTree `union` namesAtNode here) Set.empty
                (_, Just EQ, Just EQ) -> UnsolvablePrecedence def Set.empty (namesAtNode here `union` namesAtNode rTree) Set.empty
    tell err >> pure Nothing
  where
  goLeft = insertTree def lTree >>= \case
    Nothing -> pure Nothing
    Just lTree' -> pure . Just $
      Branch (Set.insert (name def) l, lTree') s (r, rTree)
  goHere = pure . Just $ Branch (l, lTree) (s `snoc` def) (r, rTree)
  goRight = insertTree def rTree >>= \case
    Nothing -> pure Nothing
    Just rTree' -> pure . Just $
      Branch (l, lTree) s (Set.insert (name def) r, rTree')

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
-- If all of them are empty, then there are no constraints on ordering.
-- If more than one is non-empty, then we must have had something in `L_x` not strictly less than something in `S_x` or `R_x`,
--   or something similar with one of `R_x` not strictly greater than something in the others.
surCmp :: Definition ann -> Set ShortText -> Either (Error ann) (Maybe Ordering)
surCmp def@MixfixDef{lowerPrecedenceThan,samePrecedenceAs,higherPrecedenceThan} names =
  let lt = lowerPrecedenceThan `intersection` names
      eq = samePrecedenceAs `intersection` names
      gt = higherPrecedenceThan `intersection` names
   in case (not $ Set.null lt, not $ Set.null eq, not $ Set.null gt) of
    (False, False, False) -> Right Nothing
    (True, False, False) -> Right (Just LT)
    (False, True, False) -> Right (Just EQ)
    (False, False, True) -> Right (Just GT)
    _ -> Left $ UnsolvablePrecedence def lt eq gt

------ Supporting Monad ------

newtype MixfixMonad ann a = MixfixMonad
  { unMonad :: (MixfixState ann, RList (Error ann)) -> (MixfixState ann, RList (Error ann), Maybe a)
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

tell :: Error ann -> MixfixMonad ann ()
tell err = MixfixMonad $ \(st, errs) -> (st, errs `snoc` err, Just ())
