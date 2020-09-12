{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

{- |
Lexing has two distinct output forms: one for colorizing source code,
and another to pass on to parsing.
The former contains too much information to be convenient for the parser,
but this module suports both.

The key idea is in the distinction between 'LexResult' and 'Token'.
The former stores the original text at all times, wheras the latter throws it away.

Lexing happens in two stages: broad and narrow.
The broad phase extracts out where token boundaries are definitely going to be,
but does not do much to categorize those tokens.
Narrow lexing analyzes each body to ensure it is a single recognizable token,
and also analyzes it in context to see if it is relevant for parsing,
or if it should take on different semantics based on nearby whitespace.
-}
module Text.Nest.Tokens.Types
    (
    -- * Payload Parts
      Payload(..)
    , Atom(..)
    , StrTemplJoin(..)
    , Side(..)
    , Separator(..)
    -- * Results
    , Result
    , Outcome(..)
    , LexResult(..)
    , LexError(..)
    -- * Location
    , Location(..)
    , startLocation
    , endLocation
    ) where

import Data.Text (Text)
import Text.Lightyear.Position (TextPos(..))


------------ Payload ------------

-- FIXME use a GADT to say which forms are allowable after narrow lexing has occurred
data Payload
    = UnknownAtom
    | Atom Atom
    | String StrTemplJoin Text StrTemplJoin
    | Bracket Side Char Char
    | UnknownSeparator
    | Separator Separator
    | Newline
    | Whitespace
    | Comment
    | Space
    | Indent Int
    deriving(Show)

data Atom
    = IntAtom Integer
    | RatAtom Rational
    | SymAtom Text
    deriving (Show)

data StrTemplJoin = Plain | Templ
    deriving(Eq, Show, Read)

data Side = Open | Close
    deriving(Eq, Show, Read)

data Separator
    = Comma
    | Dot
    | Ellipsis
    | Semicolon
    | Colon
    deriving (Show)


------------ Results ------------

type Result = LexResult Outcome

data LexResult a = LR
    { loc :: TextPos
    , orig :: Text
    , payload :: a
    }
    deriving(Functor)

data Outcome
    = Ok Payload
    | Ignore Payload
    | Error LexError

data LexError
    = BadChar TextPos Char
    | Unexpected TextPos (Maybe Char) [String] -- unexpected character/end-of-input, expected set
    | CrammedTokens TextPos Text -- tokens after first
    | MixedIndent TextPos Char -- unexpected whitespace char
    | Panic String -- for when error conditions shouldn't get thrown
    | Bundle [LexError]
    deriving (Show)

instance Semigroup LexError where
    (Panic _) <> b = b
    a <> (Panic _) = a
    (Bundle as) <> (Bundle bs) = Bundle (as <> bs)
    (Bundle as) <> b = Bundle (as ++ [b])
    a <> (Bundle bs) = Bundle (a : bs)
    a <> b = Bundle [a, b]


------------ Location ------------

data Location = Loc
    { file :: FilePath
    , from :: TextPos
    , to :: TextPos
    }

instance Show Location where
    show loc = concat [filePart, linecolPart]
        where
        filePart = concat [file loc, ": "]
        linecolPart
            | from loc == to loc = concat
                [show $ (line . from) loc, ":", show $ (col . from) loc]
            | (line . from) loc == (line . to) loc = concat
                [show $ (line . from) loc, ":", show $ (col . from) loc, "-", show $ (col . to) loc]
            | otherwise = concat
                [show $ (line . from) loc, "-", show $ (line . to) loc, ":", show $ (col . from) loc, "-", show $ (col . to) loc]


instance Semigroup Location where
    a <> b = Loc
        { file = file a
        , from = from a
        , to = to b
        }

startLocation :: Location -> Location
startLocation Loc{file,from} = Loc {file, from, to = from}

endLocation :: Location -> Location
endLocation Loc{file,to} = Loc {file, from = to, to}
