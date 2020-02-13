{-# LANGUAGE ViewPatterns #-}

module Text.Lightyear.Error
    ( MakeError
    -- * Retrive Parser State
    , ParserState
    -- $stateInfo
    , position
    , input
    , consumed
    ) where

import Prelude hiding (length)
import Text.Lightyear.Internal (ParserState(..), Stream(..))


-- | Type synonym for a continuation that creates a user-defined error value.
-- The parser state at the point of failure is made available for
-- e.g. errors that look like \"expected \'foo\', found \'bar\'\".
type MakeError st strm err = ParserState st strm -> err


-- $stateInfo
-- These functions expose part of the 'ParserState',
-- which is useful for writing informative 'MakeError' continuations.

consumed :: Stream strm => ParserState st strm -> ParserState st strm -> strm
consumed (input -> old) (input -> new) =
    let delta = length new - length old
        Just (it, _) = splitN delta old
     in it
