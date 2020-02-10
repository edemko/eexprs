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
      Atom(..)
    , StrTemplJoin(..)
    , Side(..)
    , Separator(..)
    -- * Results
    , LexResult(..)
    , LexError(..)
    -- * Location
    , Location(..)
    , startLocation
    , endLocation
    ) where

import Data.Text (Text)


------------ Payload Parts ------------

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

data LexResult a = LR
    { loc :: Location
    , orig :: Text
    , payload :: a
    }
    deriving(Functor)

data LexError = LexError
    { errLoc :: Location
    , reason :: String
    , suggestions :: [String]
    }
    deriving (Show)


------------ Location ------------

data Location = Loc
    { file :: FilePath
    , fromLine :: Int
    , fromCol :: Int
    , toLine :: Int
    , toCol :: Int
    }

instance Show Location where
    show loc = concat [filePart, linecolPart]
        where
        filePart = concat [file loc, ": "]
        linecolPart
            | fromLine loc == toLine loc
            , fromCol loc == toCol loc = concat
                [show (fromLine loc), ":", show (fromCol loc)]
            | fromLine loc == toLine loc = concat
                [show (fromLine loc), ":", show (fromCol loc), "-", show (toCol loc)]
            | otherwise = concat
                [show (fromLine loc), "-", show (toLine loc), ":", show (fromCol loc), "-", show (toCol loc)]


instance Semigroup Location where
    a <> b = Loc
        { file = file a
        , fromLine = fromLine a
        , fromCol = fromCol a
        , toLine = toLine b
        , toCol = toCol b
        }

startLocation :: Location -> Location
startLocation Loc{file,fromLine,fromCol} = Loc {file, fromLine, fromCol, toLine = fromLine, toCol = fromCol}

endLocation :: Location -> Location
endLocation Loc{file,toLine,toCol} = Loc {file, fromLine = toLine, fromCol = toCol, toLine, toCol}