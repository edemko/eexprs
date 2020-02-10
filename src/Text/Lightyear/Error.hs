module Text.Lightyear.Error
    ( MakeError
    -- * Retrive Parser State
    -- $stateInfo
    , position
    , input
    ) where

import Text.Lightyear.Internal (ParserState(..))


-- | Type synonym for a continuation that creates a user-defined error value.
-- The parser state at the point of failure is made available for
-- e.g. errors that look like \"expected \'foo\', found \'bar\'\".
type MakeError st strm err = ParserState st strm -> err


-- $stateInfo
-- These functions expose part of the 'ParserState',
-- which is useful for writing informative 'MakeError' continuations.