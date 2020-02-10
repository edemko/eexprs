{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

-- | Primitive and commonly-used parsers that operate on the prefix of the stream.
--
-- None of these parsers consume input on failure.
module Text.Lightyear.String
    ( string
    , takeWhile
    -- , takeWhile1 -- TODO
    ) where

import Prelude hiding (length,takeWhile)
import Text.Lightyear.Internal

import Text.Lightyear.Error (MakeError)


-- | Succeed and consume the prefix of the input stream
-- iff it starts with the given string.
string ::
        (Stream strm, Eq strm)
    => MakeError st strm err
    -> strm
    -> Lightyear c st strm err strm
string mkErr str = Parser $ \st -> case stateSplitN (length str) st of
    -- NOTE If you parse the same keyword several times,
    -- you'd like to share the backing memory for all those keywords.
    -- That's why it's `Ok str st'`, not `Ok str' st'`
    Just (str', st') | str == str' -> Ok str st'
    _ -> ZeroErr (mkErr st) st

-- | Consume the longest prefix of characters from the input stream
-- for which the given predicate is 'True' on each one.
--
-- This parser cannot fail: it returns an empty stream if no prefix characters
-- satisfy the predicate.
takeWhile ::
        Stream strm
    => (Chr strm -> Bool)
    -> Lightyear c st strm err strm
takeWhile p = Parser $ \st ->
    let (prefix, st') = stateSplitPred p st
     in Ok prefix st'
