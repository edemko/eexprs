{- |
See "Text.Nest.Tokens.Types".
-}
module Text.Nest.Tokens.Types.Narrow
    ( module Text.Nest.Tokens.Types
    , Payload(..)
    , Result
    , Outcome(..)
    , Token(..)
    ) where

import Text.Nest.Tokens.Types

import Data.Text (Text)

import qualified Text.Nest.Tokens.Types.Broad as Broad


type Result = LexResult Outcome

data Outcome
    = Ok Payload
    | Ignore Broad.Payload
    | Error LexError

data Token = T
    { loc :: Location
    , payload :: Payload
    }

data Payload
    = Atom Atom
    | String StrTemplJoin Text StrTemplJoin
    | Bracket Side Char Char
    | Separator Separator
    | Space
    | Indent Int
    deriving (Show)
