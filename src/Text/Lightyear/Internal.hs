{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | The guts of Lightyear's parsers.
-- Start at "Text.Lightyear" unless you know what you're doing.
module Text.Lightyear.Internal
    ( Consume(..)
    , Lightyear(..)
    , runLightyear
    , runLightyearPos
    , ParserState(..)
    , ParserInput
    , ParserResult(..)
    , Stream(..)
    , stateUncons
    , stateSplitN
    , stateSplitPred
    , Branch(..)
    ) where

import Data.Bifunctor (Bifunctor(..))
import Data.Functor ((<&>))
import Data.Kind (Type)
import Data.Proxy (Proxy(..))


-- | Meant to be used at the type level to distinguish whether
-- a given parser is intended to backtrack.
--
-- Its constructors are phantom type parameters of 'ParserResult'.
-- Look there for more detailed reasoning.
data Consume
    -- | When a @'Lightyear' 'Greedy st strm err a@ fails,
    -- the internal state may have consumed input, thereby invalidating the parser state.
    -- Attempts to try a different parser will need alternate access to a valid parser state.
    = Greedy
    -- | When a @'Lightyear' 'Atomic st strm err a@ fails,
    -- the internal state will be reset if necessary.
    -- Attempts to recover from such parsers will be able to continue using
    -- the state.
    | Atomic

-- | The abstract type for Lightyear's parsers.
-- Start at "Text.Lightyear", or look at 'ParserInput' and 'ParserResult'
-- for more internal information.
newtype Lightyear (consume :: Consume) st strm err a = Parser
    { unParser :: ParserInput st strm -> ParserResult consume st strm err a }
    -- TODO is there a way to use an STRef for the parser state rather
    -- than a plain state monad? Would that even help performance?

-- | The most general way to run a 'Lightyear' parser.
-- The user supplies the initial start position
-- along with the input and user-defined state.
runLightyearPos :: Stream strm
    => Lightyear c st strm err a -- parser to run
    -> strm -- input stream
    -> Pos strm -- start position
    -> st -- initial user-defined state
    -> Either err a
runLightyearPos action input position userState =
    case unParser action St{ input, position, userState } of
        Ok x _ -> Right x
        ZeroOk x -> Right x
        ZeroErr err -> Left err
        AdvanceErr err _ -> Left err

-- | Nearly the most general way to run a 'Lightyear' parser.
-- If you want to supply a non-default start position, see 'runLightyearPos'.
runLightyear :: forall c st strm err a. Stream strm
    => Lightyear c st strm err a -- parser to run
    -> strm -- input stream
    -> st -- initial user-defined state
    -> Either err a
runLightyear action input userState =
    let position = startPosition (Proxy @ strm)
     in runLightyearPos action input position userState


-- | The internal state of Lightyear's parsers.
-- It is parameterized by a user-defined state type @st@ an input stream type @strm@.
data ParserState st strm = St
    { input :: strm -- ^ remaining input
    , position :: Pos strm -- ^ current position through the input
    , userState :: st -- ^ user-defined state
    }

instance (Eq strm, Eq st) => Eq (ParserState st strm) where
    a == b = input a == input b && userState a == userState b
    -- TODO I could have a sort of buffer
    -- Have the entire input, and another type which is the current position in the input,
    -- including a slice into it.
    -- Then, taking a character at a time can just increment the offset,
    -- but when larger pieces of input are taken, we can create a thunk
    -- to store the updated position.


-- | Just a type synonym for internal use that gives a hint as to why the state is being used.
-- That is, I like the symmetry with 'ParserResult' in 'Parser'.
type ParserInput st strm = ParserState st strm

-- | The internal type representing parser success or failure, and what can be done to recover.
data ParserResult (consume :: Consume) st strm err a {-= (Either err a, ParserState st strm) -} where
    -- | The parser succeeded and consumed input or altered the user state.
    --
    -- Note the polymorphism in the 'Consume' type argument.
    -- If the parser succeeded, it doesn't matter whether input was consumed.
    Ok :: a -- ^ user result
        -> ParserState st strm -- ^ updated state
        -> ParserResult c st strm err a
    -- | The parser succeeded without consuming input or altering the user state.
    --
    -- This is particularly useful for guarding against infinite loops
    -- in 'many' and 'some'.
    ZeroOk :: a -- ^ user result
        -> ParserResult c st strm err a
    -- | The parser failed, but neither consumed input nor changed the user state.
    --
    -- Note the polymorphism in the 'Consume' type argument.
    -- 'ZeroErr' may appear in a 'Greedy' parser as well as a 'Atomic' one.
    -- When it does so, even a 'Greedy' parser is able to recover.
    ZeroErr :: err -- ^ error information
        -> ParserResult c st strm err a
    -- | The parser failed, and consumed (i.e. invalidated) the parser state,
    -- either by consuming input or updating the user state.
    AdvanceErr :: err -- ^ error information
        -> ParserState st strm -- ^ the /invalid/ state (see warning above)
        -> ParserResult 'Greedy st strm err a

-- | Typeclass for inputs that Lightyear can consume.
--
-- The goal is to allow taking one or many elements ('Chr') off the front of the 'Stream' efficiently.
-- Parsers expect reasonable complexity for front-consumable streams (see the documentation for each method).
-- If these requirements are not met, it is likely that parsers over such 'Streams' will have poor performance.
-- So, although you might be able to write a '[]' instance, it's probably not the best idea to actually use it.
class Stream strm where
    -- | The type of characters in the stream.
    -- E.g. 'Char' for 'String' or 'Data.Text.Text', 'Data.Word.Word8' for 'Data.ByteString.ByteString'.
    type Chr strm :: Type

    -- | The type of positions in the stream
    -- E.g. line and column info for text formats, byte position for binary formats.
    -- See 'Text.Lightyear.Position' for some common position types.
    type Pos strm :: Type

    -- | returns the head and tail of the input stream
    --
    -- This should complete in @O(1)@ time.
    uncons :: strm -> Maybe (Chr strm, strm)

    -- | @'splitN' n str@ returns the first @n@ tokens in @str@,
    -- but only if @str@ has that many characters.
    -- Also returns the remainder after the prefix has been taken away.
    --
    -- This should complete with @O(1)@ allocations.
    splitN :: Int -> strm -> Maybe (strm, strm)

    -- | Split the stream into @(prefix, suffix)@ where @prefix@ is the longest
    -- prefix of the input for which all tokens satisfy the predicate.
    --
    -- This should complete with @O(1)@ allocations.
    splitPred :: (Chr strm -> Bool) -> strm -> (strm, strm)

    -- | Get the length of the stream in number of @Chr strm@ elements.
    --
    -- This should complete in @O(1)@ time.
    length :: strm -> Int

    -- | The default initial position used by 'runLightyear'
    startPosition :: Proxy strm -> Pos strm

    -- | Given a single character from the stream, update the input position.
    advanceOne :: Proxy strm -> Chr strm -> Pos strm -> Pos strm

    -- | Given a portion of the stream, update the input position.
    -- If 'Pos' is simple enough, this should be much faster than folding 'advanceOne'.
    advance :: strm -> Pos strm -> Pos strm
    advance strm !pos = case uncons strm of
        Just (c, rest) -> advance rest (advanceOne (Proxy @strm) c pos)
        Nothing -> pos

-- | Only for use building primitive combinators
--      as in "Text.Lightyear.Char" and "Text.Lightyear.String".
-- Lifts 'uncons' over @strm@ to operate over @'ParserState st strm'@.
stateUncons :: forall st strm.
        Stream strm
    => ParserState st strm
    -> Maybe (Chr strm, ParserState st strm)
stateUncons st@St{input,position} = case uncons input of
    Just (c, str) ->
        let st' = st{input = str, position = advanceOne (Proxy @strm) c position}
         in Just (c, st')
    Nothing -> Nothing

-- | Only for use building primitive combinators
--      as in "Text.Lightyear.Char" and "Text.Lightyear.String".
-- Lifts 'splitN' over @strm@ to operate over @'ParserState st strm'@.
stateSplitN ::
        Stream strm
    => Int
    -> ParserState st strm
    -> Maybe (strm, ParserState st strm)
stateSplitN n st@St{input,position} = case splitN n input of
    Just (prefix, suffix) ->
        let st' = st{input = suffix, position = advance prefix position}
         in Just (prefix, st')
    Nothing -> Nothing

-- | Only for use building primitive combinators
--      as in "Text.Lightyear.Char" and "Text.Lightyear.String".
-- Lifts 'splitPred' over @strm@ to operate over @'ParserState st strm'@.
stateSplitPred ::
        Stream strm
    => (Chr strm -> Bool)
    -> ParserState st strm
    -> (strm, ParserState st strm)
stateSplitPred p st@St{input,position} =
    let (prefix, suffix) = splitPred p input
        st' = st{input = suffix, position = advance prefix position}
     in (prefix, st')


------------ Typeclass Instances ------------

instance Functor (ParserResult c st strm err) where
    fmap f = \case
        Ok x st -> Ok (f x) st
        ZeroOk x -> ZeroOk (f x)
        ZeroErr err -> ZeroErr err
        AdvanceErr err st -> AdvanceErr err st

instance Functor (Lightyear c st strm err) where
    fmap = second

instance Bifunctor (Lightyear c st strm) where
    bimap g f action = Parser $ \st -> case unParser action st of
        Ok x st' -> Ok (f x) st'
        ZeroOk x -> ZeroOk (f x)
        ZeroErr err -> ZeroErr (g err)
        AdvanceErr err st' -> AdvanceErr (g err) st'

instance Applicative (Lightyear 'Greedy st strm err) where
    pure x = Parser $ \_ -> ZeroOk x
    fA <*> xA = Parser $ \st -> case unParser fA st of
        Ok f st' -> case unParser xA st' of
            Ok x st'' -> Ok (f x) st''
            ZeroOk x -> Ok (f x) st'
            ZeroErr err -> AdvanceErr err st'
            AdvanceErr err st'' -> AdvanceErr err st''
        ZeroOk f -> case unParser xA st of
            Ok x st' -> Ok (f x) st'
            ZeroOk x -> ZeroOk (f x)
            ZeroErr err -> ZeroErr err
            AdvanceErr err st' -> AdvanceErr err st'
        ZeroErr err -> ZeroErr err
        AdvanceErr err st' -> AdvanceErr err st'
instance Applicative (Lightyear 'Atomic st strm err) where
    pure x = Parser $ \_ -> ZeroOk x
    fA <*> xA = Parser $ \st -> case unParser fA st of
        Ok f st' -> case unParser xA st' of
            Ok x st'' -> Ok (f x) st''
            ZeroOk x -> Ok (f x) st'
            ZeroErr err -> ZeroErr err
        ZeroOk f -> case unParser xA st of
            Ok x st' -> Ok (f x) st'
            ZeroOk x -> ZeroOk (f x)
            ZeroErr err -> ZeroErr err
        ZeroErr err -> ZeroErr err

instance (Semigroup err) => Branch (Lightyear 'Greedy st strm err) where
    a <|> b = Parser $ \st -> case unParser a st of
        Ok x st' -> Ok x st'
        ZeroOk x -> ZeroOk x
        ZeroErr err -> case unParser b st of
            Ok y st' -> Ok y st'
            ZeroOk y -> ZeroOk y
            ZeroErr err' -> ZeroErr (err <> err')
            AdvanceErr err' st' -> AdvanceErr (err <> err') st'
        AdvanceErr err st' -> AdvanceErr err st'
    -- FIXME if I add a ZeroOk ctor to ParserResult, then this can abort when an infinite loop is detected in `many` (probably using an exception)
    many action = Parser $ \st -> case unParser action st of
        Ok x st' -> (x:) <$> loop st'
        ZeroOk _ -> error "infinite loop detected in parser"
        ZeroErr _ -> ZeroOk []
        AdvanceErr _ _ -> ZeroOk []
        where
        loop st = case unParser action st of
            Ok x st' -> (x:) <$> loop st'
            ZeroOk _ -> error "infinite loop detected in parser"
            ZeroErr _ -> Ok [] st
            AdvanceErr _ _ -> Ok [] st
    some action = Parser $ \st -> case unParser action st of
        Ok x st' -> (x:) <$> loop st'
        ZeroOk _ -> error "infinite loop detected in parser"
        ZeroErr err -> ZeroErr err
        AdvanceErr err st' -> AdvanceErr err st'
        where
        loop st = case unParser action st of
            Ok x st' -> (x:) <$> loop st'
            ZeroOk _ -> error "infinite loop detected in parser"
            ZeroErr _ -> Ok [] st
            AdvanceErr _ _ -> Ok [] st
instance (Semigroup err) => Branch (Lightyear 'Atomic st strm err) where
    a <|> b = Parser $ \st -> case unParser a st of
        Ok x st' -> Ok x st'
        ZeroOk x -> ZeroOk x
        ZeroErr err -> case unParser b st of
            Ok y st' -> Ok y st'
            ZeroOk y -> ZeroOk y
            ZeroErr err' -> ZeroErr (err <> err')
    many action = Parser $ \st -> case unParser action st of
        Ok x st' -> (x:) <$> loop st'
        ZeroOk _ -> error "infinite loop detected in parser"
        ZeroErr _ -> ZeroOk []
        where
        loop st = case unParser action st of
            Ok x st' -> (x:) <$> loop st'
            ZeroOk _ -> error "infinite loop detected in parser"
            ZeroErr _ -> Ok [] st
    some action = Parser $ \st -> case unParser action st of
        Ok x st' -> (x:) <$> loop st'
        ZeroOk _ -> error "infinite loop detected in parser"
        ZeroErr err -> ZeroErr err
        where
        loop st = case unParser action st of
            Ok x st' -> (x:) <$> loop st'
            ZeroOk _ -> error "infinite loop detected in parser"
            ZeroErr _ -> Ok [] st

instance Monad (Lightyear 'Greedy st strm err) where
    return = pure
    action >>= k = Parser $ \st -> case unParser action st of
        Ok x st' -> case unParser (k x) st' of
            Ok y st'' -> Ok y st''
            ZeroOk y -> Ok y st'
            ZeroErr err -> AdvanceErr err st'
            AdvanceErr err st'' -> AdvanceErr err st''
        ZeroOk x -> unParser (k x) st
        ZeroErr err -> ZeroErr err
        AdvanceErr err st'' -> AdvanceErr err st''
instance Monad (Lightyear 'Atomic st strm err) where
    return = pure
    action >>= k = Parser $ \st -> case unParser action st of
        Ok x st' -> unParser (k x) st'
        ZeroOk x -> unParser (k x) st
        ZeroErr err -> ZeroErr err


------------ Branch Typeclass ------------

-- Branch is to Alternative as Semigroup is to Monoid
-- | A semigroup on applicative functors.
--
-- If defined, 'some' and 'many' should be the least solutions of the equations:
--
-- * @'some' v = (:) '<$>' v '<*>' 'many' v@
-- * @'many' v = 'some' v '<|>' 'pure' []@
--
--  There is no meaningful instance of 'Control.Applicative.Alternative' for 'Lightyear'
--      because 'Control.Applicative.empty' has no meaningful implementation when the error type is fully-custom.
-- Instead, we define the very similar 'Branch' class, which bears the same relationship to 'Control.Applicative.Alternative'
--      as 'Semigroup' bears to 'Monoid'.
-- That is, it has no zero element.
class Applicative f => Branch f where
    -- | An associative binary operation
    (<|>) :: f a -> f a -> f a
    -- | One or more.
    some :: f a -> f [a]
    some x = (x <&> (:)) <*> many x
    -- | Zero or more.
    many :: f a -> f [a]
    many x = some x <|> pure []
