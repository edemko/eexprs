{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

-- | Primitive and commonly-used parsers that operate on the stream one character at a time.
--
-- None of these parsers consume input on failure.
module Text.Lightyear.Char
    ( satisfy
    -- , any -- TODO
    -- , char -- TODO
    ) where

import Text.Lightyear.Internal

import Text.Lightyear.Error (MakeError)


-- | Succeeds and consumes the first character from the input stream
-- iff the given predicate returns 'True'.
satisfy ::
        Stream strm
    => MakeError st strm err
    -> (Chr strm -> Bool)
    -> Lightyear c st strm err (Chr strm)
satisfy mkErr p = Parser $ \st -> case stateUncons st of
    Just (c, st') | p c -> Ok c st'
    _ -> ZeroErr (mkErr st) st
