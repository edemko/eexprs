{- |
See "Text.Nest.Tokens.Types".
-}
module Text.Nest.Tokens.Types.Broad
    ( module Text.Nest.Tokens.Types
    , Payload(..)
    , Result
    ) where

import Text.Nest.Tokens.Types

import Data.Text (Text)


data Payload
    = Atom
    | String StrTemplJoin Text StrTemplJoin
    | Bracket Side Char Char
    | Separator
    | Newline
    | Whitespace
    | Comment
    deriving(Show)

type Result = LexResult (Either LexError Payload)
