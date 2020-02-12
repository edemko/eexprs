{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}

module Text.Lightyear.Combinators
    (
    -- * Alternation
    -- $branch
      Branch(..)
    , choice
    , option
    , option_
    -- , choice0 -- TODO takes a plain list
    -- * Sequencing
    -- $sequence
    -- * Iteration
    -- TODO
    -- * Error Management
    , try
    , commit
    , label
    , fail
    -- , ensureMoved -- TODO
    -- * Miscellaneous
    , endOfInput
    -- ** Look Around
    , lookAhead
    , notFollowedBy
    -- * State
    -- ** User State
    , getState
    , setState
    , modifyState
    -- ** Source Position
    , getPosition -- TODO
    ) where

import Prelude hiding (fail)

import Data.List.NonEmpty (NonEmpty((:|)))
import Text.Lightyear.Internal
import Text.Lightyear.Error (MakeError)


------------ Primitives ------------

-- | Succeeds only when the end of input was reached.
endOfInput ::
        Stream strm
    => MakeError st strm err
    -> Lightyear c st strm err ()
endOfInput mkErr = Parser $ \st -> case uncons (input st) of
    Nothing -> Ok () st
    Just (_, _) -> ZeroErr (mkErr st) st

-- | Stop here and report an error.
fail :: MakeError st strm err -> Lightyear c st strm err a
fail mkErr = Parser $ \st -> ZeroErr (mkErr st) st


------------ Alternation ------------

-- $branch
--  Alternation between parsers is based primarily on the 'Branch'
-- typeclass, which generalizes 'Alternative'.
--
-- One wrinkle in this is that using 'many' or 'some' on a 'Consuming'
-- parser will perform only about as well as a 'Backtracking' parser.
-- This is because if we get some successes, a purely-comsuming parser
-- would throw them all away at the first failure (i.e. the end of the
-- repetition has ceased).
-- Therefore, once a 'Consuming' parser has successfully parsed a single
-- @p@ in @'many' p@ or @'some' p@, it will then carry on as if it were a
-- backtracking parser.

choice :: Branch (Lightyear c st strm err)
    => NonEmpty (Lightyear c st strm err a)
    -> Lightyear c st strm err a
choice (x :| []) = x
choice (x :| xs) = foldr1 (<|>) (x:xs)


option :: Branch (Lightyear c st strm err)
    => a
    -> Lightyear c st strm err a
    -> Lightyear c st strm err a
option def action = action <|> pure def

option_ :: Branch (Lightyear c st strm err)
    => Lightyear c st strm err a
    -> Lightyear c st strm err ()
option_ action = option () (() <$ action)


------------ Iteration ------------

-- $sequence
-- Sequencing of parsers is done mostly with 'Applicative' and 'Monad'.
-- In @a '<*>' b@ and @a '>>=' b@, parser @a@ is performed first,
-- and if it succeeds parsing continues with @b@.
-- As normal, the monadic interface allows @b@ to depend on the
-- result of @a@, wheras the applicative interface does not.
-- Thus, context-free grammars can be described by applicatives, such as
--
-- @Lambda <$> variableName <*> expression@
--
-- but context-sensitivity requires monads.
--
-- There are plenty of fun ways to play with these APIs.
-- Try 'Data.Functor.<&>',
--  'Data.Functor.<$', 'Data.Functor.$>',
--  'Data.Functor.void',
--  'Control.Applicative.<**>', '<*', and '*>'!


------------ Modified Movement ------------

-- | Create an explicit backtrack point.
-- If the input parser fails, this parser will not consume input.
try :: Lightyear 'Consuming st strm err a -> Lightyear 'Backtracking st strm err a
try action = Parser $ \st -> case unParser action st of
    Ok x st' -> Ok x st'
    ZeroErr err st' -> ZeroErr err st'
    -- NOTE keeping the original `st` around for this constructor is why `try` adds expense
    AdvanceErr err -> ZeroErr err st

-- | Turn backtracking parser to a consuming parser.
-- Although the result is a consuming parser, since the input is backtracking,
-- backtracking will still be able to if this parser fails.
-- The point here is that ,if one wanted to combine a backtracking with a
--      consuming parser to create a consuming parser,
--      this would be necessary to adapt the types.
--
-- Although the resulting parser could be polymorphic in 'Consume',
-- it doesn't make much sense to 'commit' on an already 'Consuming' parser,
-- since they implicity commit for every operation.
commit :: Lightyear 'Backtracking st strm err a -> Lightyear 'Consuming st strm err a
commit action = Parser $ \st -> case unParser action st of
    Ok x st' -> Ok x st'
    -- NOTE this is why ZeroErr has a state and AdvaceErr doesn't: commit will throw away the pointer to the original state
    ZeroErr err st' -> ZeroErr err st'

-- | If the first parser fails with some error /without consuming input/,
-- construct a new error to replace the original.
-- The continuation that creates the new error has access to
-- both the old error as well as the parser state at the point where 'label' is called.
label :: (err -> MakeError st strm err)
    -> Lightyear c st strm err a
    -> Lightyear c st strm err a
label mkErr action = Parser $ \st -> case unParser action st of
    Ok x st' -> Ok x st'
    ZeroErr err _ -> ZeroErr (mkErr err st) st
    AdvanceErr err -> AdvanceErr err

-- | Try the given parser here, and succeed exactly when it does.
--
-- It generally doesn't make sense to look ahead with a 'Consuming' parser,
-- since the state will have to be reset if it succeeds anyway.
lookAhead :: Lightyear 'Backtracking st strm err a -> Lightyear c st strm err a
lookAhead action = Parser $ \st -> case unParser action st of
    Ok x _ -> Ok x st
    ZeroErr err _ -> ZeroErr err st

-- | Like 'lookAhead', but fail when the given parser succeeds and vice-versa.
notFollowedBy ::
        (a -> err)
    -> Lightyear 'Backtracking st strm err a
    -> Lightyear c st strm err ()
notFollowedBy mkErr action = Parser $ \st -> case unParser action st of
    Ok x _ -> ZeroErr (mkErr x) st
    ZeroErr _ _ -> Ok () st

-- TODO the same performance implications as 'try'
-- TODO if I get a ZeroOk ctor, then I can make `many` do this sort of thing automatically, even without `Eq` instances
ensureMoved :: (Eq st, Eq strm) => err -> Lightyear c st strm err a -> Lightyear c st strm err a 
ensureMoved mkErr action = Parser $ \st -> case unParser action st of
    Ok x st' ->
        if st == st'
        then ZeroErr mkErr st' -- NOTE don't return `st`, return `st'`; I hope the compiler can figure out `st` doesn't escape
        else Ok x st'
    ZeroErr err st' -> ZeroErr err st'
    AdvanceErr err -> AdvanceErr err


------------ State ------------

-- | Retrieve the current value of the user-defined state.
-- Lightyear never modifies this on its own (except to backtrack).
-- See 'setState' and 'modifyState' for manipulating state.
getState :: Lightyear c st strm err st
getState = Parser $ \st -> Ok (userState st) st

-- | Replace the current value of the user-defined state with a new state.
-- Lightyear never modifies user state on its own (except to backtrack).
-- See 'getState' for using this state later.
setState :: st -> Lightyear c st strm err ()
setState userState = Parser $ \st -> Ok () st{userState}

-- | Alter the current value of the user-defined state with an endomorphism.
-- Returns the value of the updated state.
-- Lightyear never modifies user state on its own (except to backtrack).
-- See 'getState' for using this state later.
modifyState :: (st -> st) -> Lightyear c st strm err st
modifyState f = Parser $ \st@St{userState} ->
    let userState' = f userState
     in Ok userState' st{userState = userState'}

-- | Retrive the current position in the input stream.
getPosition :: Lightyear c st strm err (Pos strm)
getPosition = Parser $ \st -> Ok (position st) st
