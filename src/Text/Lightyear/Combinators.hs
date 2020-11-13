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
    , many1
    -- * Iteration
    , sepBy
    -- TODO sepBy variants
    -- * Error Management
    , try
    , recover
    , fromAtomic
    , unsafeToAtomic
    , label
    , fail
    , advancing
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
    Nothing -> ZeroOk ()
    Just (_, _) -> ZeroErr (mkErr st)

-- | Stop here and report an error.
fail :: MakeError st strm err -> Lightyear c st strm err a
fail mkErr = Parser $ \st -> ZeroErr (mkErr st)


------------ Alternation ------------

-- $branch
--  Alternation between parsers is based primarily on the 'Branch'
-- typeclass, which generalizes 'Alternative'.
--
-- One wrinkle in this is that using 'many' or 'some' on a 'Greedy'
-- parser will perform only about as well as a 'Atomic' parser.
-- This is because if we get some successes, a purely-comsuming parser
-- would throw them all away at the first failure (i.e. the end of the
-- repetition has ceased).
-- Therefore, once a 'Greedy' parser has successfully parsed a single
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

many1 :: Branch (Lightyear c st strm err)
    => Lightyear c st strm err a
    -> Lightyear c st strm err a
    -> Lightyear c st strm err [a]
many1 first rest = Parser $ \st -> case unParser first st of
    Ok x st' -> case unParser (many rest) st' of
        Ok xs st'' -> Ok (x : xs) st''
        ZeroOk xs -> Ok (x : xs) st'
        ZeroErr _ -> error "Lightyear Internal Error: `many` failed"
        AdvanceErr _ _ -> error "Lightyear Internal Error: `many` failed"
    ZeroOk x -> case unParser (many rest) st of
        Ok xs st' -> Ok (x : xs) st'
        ZeroOk xs -> ZeroOk (x : xs)
        ZeroErr _ -> error "Lightyear Internal Error: `many` failed"
        AdvanceErr _ _ -> error "Lightyear Internal Error: `many` failed"
    ZeroErr _ -> ZeroOk []
    AdvanceErr _ _ -> ZeroOk []

-- TODO some1; as many1, but the first is mandatory (just for symmetry)

sepBy :: (Branch (Lightyear 'Greedy st strm err))
    => Lightyear 'Greedy st strm err a -- ^ the parser to repeat
    -> Lightyear 'Greedy st strm err b -- ^ the parser to separate with
    -> Lightyear 'Greedy st strm err [a]
sepBy body sep = many1 body (sep >> body)

------------ Modified Movement ------------

-- | Create an explicit backtrack point.
-- If the input parser fails, this parser will not consume input.
try :: Lightyear 'Greedy st strm err a -> Lightyear 'Atomic st strm err a
try action = Parser $ \st -> case unParser action st of
    Ok x st' -> Ok x st'
    ZeroOk x -> ZeroOk x
    ZeroErr err -> ZeroErr err
    -- NOTE keeping the original `st` around for this constructor is why `try` adds expense
    AdvanceErr err _ -> ZeroErr err

recover :: Lightyear c st strm err a -> Lightyear c st strm err (Either err a)
recover action = Parser $ \st -> case unParser action st of
    Ok x st' -> Ok (Right x) st'
    ZeroOk x -> ZeroOk (Right x)
    ZeroErr err -> ZeroOk (Left err)
    AdvanceErr err st' -> Ok (Left err) st'

-- | Adapt an atomic parser for use in greedy contexts.
-- Although the result is a greedy parser, since the input is atomic,
-- atomic will still be able to if this parser fails.
-- The point here is that ,if one wanted to combine a atomic with a
--      greedy parser to create a greedy parser,
--      this would be necessary to adapt the types.
--
-- Although the resulting parser could be polymorphic in 'Consume',
-- it doesn't make much sense to 'fromAtomic' an 'Atomic' parser to
-- another 'Atomic' parser.
fromAtomic :: Lightyear 'Atomic st strm err a -> Lightyear 'Greedy st strm err a
fromAtomic action = Parser $ \st -> case unParser action st of
    Ok x st' -> Ok x st'
    ZeroOk x -> ZeroOk x
    ZeroErr err -> ZeroErr err

unsafeToAtomic :: Lightyear 'Greedy st strm err a -> Lightyear 'Atomic st strm err a
unsafeToAtomic action = Parser $ \st -> case unParser action st of
    Ok x st' -> Ok x st'
    ZeroOk x -> ZeroOk x
    ZeroErr err -> ZeroErr err
    AdvanceErr _ _ -> error "unsafeToAtomic"

-- | If the first parser fails with some error /without consuming input/,
-- construct a new error to replace the original.
-- The continuation that creates the new error has access to
-- both the old error as well as the parser state at the point where 'label' is called.
label :: (err -> MakeError st strm err)
    -> Lightyear c st strm err a
    -> Lightyear c st strm err a
label mkErr action = Parser $ \st -> case unParser action st of
    Ok x st' -> Ok x st'
    ZeroOk x -> ZeroOk x
    ZeroErr err -> ZeroErr (mkErr err st)
    AdvanceErr err st' -> AdvanceErr err st'

-- | Try the given parser here, and succeed exactly when it does.
--
-- It generally doesn't make sense to look ahead with a 'Greedy' parser,
-- since the state will have to be reset if it succeeds anyway.
lookAhead :: Lightyear 'Atomic st strm err a -> Lightyear c st strm err a
lookAhead action = Parser $ \st -> case unParser action st of
    Ok x _ -> Ok x st
    ZeroOk x -> ZeroOk x
    ZeroErr err -> ZeroErr err

-- | Like 'lookAhead', but fail when the given parser succeeds and vice-versa.
notFollowedBy ::
        (a -> err)
    -> Lightyear 'Atomic st strm err a
    -> Lightyear c st strm err ()
notFollowedBy mkErr action = Parser $ \st -> case unParser action st of
    Ok x _ -> ZeroErr (mkErr x)
    ZeroOk x -> ZeroErr (mkErr x)
    ZeroErr _ -> ZeroOk ()

-- TODO the same performance implications as 'try'
-- Run the given parser, but also require the stream to advance
advancing :: err -> Lightyear c st strm err a -> Lightyear c st strm err a 
advancing mkErr action = Parser $ \st -> case unParser action st of
    Ok x st' -> Ok x st'
    ZeroOk _ -> ZeroErr mkErr
    ZeroErr err -> ZeroErr err
    AdvanceErr err st' -> AdvanceErr err st'


------------ State ------------

-- | Retrieve the current value of the user-defined state.
-- Lightyear never modifies this on its own (except to backtrack).
-- See 'setState' and 'modifyState' for manipulating state.
getState :: Lightyear c st strm err st
getState = Parser $ \st -> ZeroOk (userState st)

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
getPosition = Parser $ \st -> ZeroOk (position st)
