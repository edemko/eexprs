-- | General-purpose position types.
module Text.Lightyear.Position
    ( TextPos(..)
    , startTextPos
    , BytePos(..)
    , startBytePos
    , FilePos(..)
    , WithFilePath(..)
    ) where


-- | Position type for byte-oriented input streams.
newtype BytePos = BytePos
    { byte :: Int -- ^ (zero-indexed) offset into the input stream
    }
    deriving (Eq, Ord, Show, Read)

-- | The default initial position for byte-oriented input.
-- Starts at zero.
startBytePos :: BytePos
startBytePos = BytePos 0


-- | Position type for text-oriented input streams
data TextPos = TextPos
    { line :: {-# UNPACK #-} !Int -- ^ line number (one-indexed)
    , col :: {-# UNPACK #-} !Int -- ^ column number (zero-indexed)
    }
    deriving (Eq, Ord, Show, Read)

-- | The default initial position for text-oriented input.
-- Starts at line one, column zero
startTextPos :: TextPos
startTextPos = TextPos{line = 1, col = 1}


-- | Attach a file name to an existing position type.
data FilePos a = FilePos
    { filename :: FilePath
    , pos :: !a
    }
    deriving (Eq, Ord, Show, Read)

-- | Sometimes, it makes sense to attach a filename to a position,
-- but when accepting input from, say, a REPL, there is no sensible file name.
-- Lightyear defaults to not tracking filename as part of the position.
--
-- This newtype can wrap a stream, which attaches the file name
-- using 'FilePos'.
newtype WithFilePath a = WithFilePath { unFilePath :: a }


-- TODO Range a = Range { from :: a, to :: a }
-- TODO WithRanges a = WithRanges { unRanges :: a }
-- TODO startRange start = Range { from = start, to = start }
