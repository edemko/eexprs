-- | Lightyear is a parser combinator library that elegantly flows between performance and correctness.
-- 
-- Parser combinators face a choice on failure between comsuming input or backtracking.
-- If the former, then
--      in the successful branches, the stream can be consumed and the memory footprint can stay smaller,
--      but unusual errors can be generated if the parser writer doesn't use 'try' in the right places.
-- If the latter, then
--      it's much easier to write correct parsers, but
--      unless the writer uses 'commit' in just the right places,
--      the entire input will have to be kept in memory until parsing is complete.
--
-- In this library, every parser is annotated at the type level with one of 'Consuming' or 'Backtracking'.
-- With this information available, you can now leverage the compiler to discover where 'try' and 'commit' should be inserted.
-- Less sensitive users can still pick one of the strategies above by limiting themselves to one type of parser.
--
-- Lightyear also parameterizes the error and position types for parsers.
-- On many occaisions a library's fixed position types are unsuitable
--     (e.g. taking input from non-file sources shouldn't have a filename;
--     parsing bytes should just give an offset not line:col).
-- Here, the 'Stream' class has an accosiated 'Pos' type.
-- Further, although offering a built-in error type (even one which has a custom error constructor)
--     is more annoying to work with than a fully-custom error type.
-- Admittedly, there's a tradeoff here: a fully-custom error type means that
--     Lightyear cannot generate errors for the parser writer automatically;
--     instead, many combinators take an additional 'MakeError' argument for constructing your error.
-- On the other hand, fully-custom errors can be very explicit about what exactly went wrong,
--     allowing error consumers to gernerate good suggestions.
--
-- Another possibly controversial choice is the absence of a @LightyearT@ monad transformer.
-- Since Lightyear's combinators might backtrack, so too must any monads stacked under[?] a hypothetical @LightyearT@,
--     but this is not possible in general (e.g. @'Lightyear' 'Backtrack st strm err IO result@).
-- (Editors note:
-- Honestly, I don't even know how to understand what a parser monad transformer does with its base monad, even when it /is/ possible to backtrack.
-- Would you /really/ want to use a library feature it's author didn't understand?)
-- Instead, we offer a user-defined state type (the @st@ in @'Lightyear' st strm err a@),
--     and humbly suggest to move any side-effects towards the periphery of your program
--     instead of burying them inside the input validator.
-- Of course, running 'Lightyear' under another monad transformer /is/ straightforward and sometimes useful
--     (e.g. @'Control.Monad.Reader.ReaderT' Config ('Lightyear' c st strm err) a@).
--
-- Lightyear has no relation to the [Idris parser combinator library of the same name](https://github.com/ziman/lightyear) (though we are both inspired by Parsec).
module Text.Lightyear
    ( Lightyear
    , runLightyear
    , runLightyearPos
    , Consume(..)
    , module X
    ) where

import Text.Lightyear.Internal

import Text.Lightyear.Char as X
import Text.Lightyear.String as X
import Text.Lightyear.Combinators as X
import Text.Lightyear.Error as X
import Text.Lightyear.Position as X
import Text.Lightyear.Stream ()
