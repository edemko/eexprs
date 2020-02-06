{-# LANGUAGE NamedFieldPuns #-}

module Text.Nest.Tokens
    ( Token(..)
    , TokenPayload(..)
    , Atom(..)
    , Side(..)
    , StrTemplJoin(..)
    , Location(..)
    , startLocation
    , endLocation
    ) where

import Data.Text (Text)

import qualified Data.Text as T


data Token = Token
    { loc :: Location
    , payload :: TokenPayload
    }

data TokenPayload
    = Atom Atom
    | String StrTemplJoin Text StrTemplJoin
    | Bracket Side Char Char
    | Whitespace
    | Indent Int
    | EndOfFile
    | Comment
    | UnknownAtom Text
    | UnknownSeparator Text
    | RecognizedSeparator Text
    | UnknownNewline
    | UnknownWhitespace Text
    | UnknownIndent Text
    deriving (Show)


data Atom
    = IntAtom Integer
    | RatAtom Rational
    | SymAtom Text
    deriving (Show)

data StrTemplJoin = Plain | Templ
    deriving(Eq, Show, Read)
data Side = Open | Close
    deriving(Eq, Show, Read)


data Location = Loc
    { file :: FilePath
    , fromLine :: Int
    , fromCol :: Int
    , toLine :: Int
    , toCol :: Int
    }

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