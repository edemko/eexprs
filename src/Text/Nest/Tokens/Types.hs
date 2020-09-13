{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
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
    , Combiner(..)
    , Separator(..)
    -- * Results
    , Phase(..)
    , Result
    , Outcome(..)
    , LexResult(..)
    , LexError(..)
    , SomeResult
    , SomeOutcome(..)
    -- * Location
    , Location(..)
    , startLocation
    , endLocation
    ) where

import Data.Kind

import Data.Text (Text)
import GHC.Show (showSpace)
import Text.Lightyear.Position (TextPos(..))


------------ Payload ------------

data Phase = Free | Sens

data Payload :: Phase -> Type where
    Atom :: Atom -> Payload phase
    String :: StrTemplJoin -> Text -> StrTemplJoin -> Payload phase
    Combiner :: Side -> Combiner phase -> Payload phase
    Separator :: Separator phase -> Payload phase
    ChainDot :: Payload 'Sens
    SyntheticDot :: Payload 'Sens
    StartIndent :: Payload 'Free -- FIXME not actually allowable except in the midst of context-sensitive parsing
    Comment :: Payload 'Free
    UnknownSpace :: Payload 'Free
    UnknownNewline :: Payload 'Free
    UnknownDot :: Payload 'Free -- FIXME rename to UnknownDot
    UnknownColon :: Payload 'Free -- FIXME rename to UnknownColon

data Atom
    = IntAtom Integer
    | RatAtom Rational
    | SymAtom Text
    deriving (Show)

data StrTemplJoin = Plain | Templ
    deriving(Eq, Show, Read)

data Side = Open | Close
    deriving(Eq, Show, Read)

data Combiner :: Phase -> Type where
    Paren :: Combiner phase
    Brack :: Combiner phase
    Brace :: Combiner phase
    Indent :: Combiner 'Sens

data Separator :: Phase -> Type where
    Comma :: Separator phase
    Dot :: Separator 'Sens
    Ellipsis :: Separator phase
    Semicolon :: Separator phase
    Colon :: Separator 'Sens
    Space :: Separator 'Sens
    Newline :: Separator 'Sens


------------ Results ------------

type Result phase = LexResult (Outcome phase)

type SomeResult = LexResult SomeOutcome
data SomeOutcome = forall phase. SO (Outcome phase)

data LexResult a = LR
    { loc :: TextPos
    , orig :: Text
    , payload :: a
    }
    deriving(Functor,Show)

data Outcome :: Phase -> Type where
    Ok :: Payload phase -> Outcome phase
    Ignore :: Payload phase -> Outcome 'Sens
    Error :: LexError -> Outcome phase

data LexError
    = BadChar TextPos Char
    | Unexpected TextPos (Maybe Char) [String] -- unexpected character/end-of-input, expected set
    | CrammedTokens TextPos Text -- tokens after first
    | MixedIndent TextPos Char -- unexpected whitespace char
    | IllegalDot -- dot with whitespace after, but not before
    | IllegalIndent -- if file starts with indent, or indent is smaller than previous level
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

------------ Show Instances ------------

instance Show (Outcome phase) where
    showsPrec p tok = showParen (p >= 11) $
        case tok of
            Ok x -> showString "Ok " . showsPrec 11 x
            Ignore x -> showString "Ignore " . showsPrec 11 x
            Error err -> showString "Error " . showsPrec 11 err

instance Show (Payload phase) where
    showsPrec p tok = showParen (p >= 11) $
        case tok of
            Atom a -> showString "Atom " . showsPrec 11 a
            String o str c -> showString "String " . showsPrec 11 o . showSpace . showsPrec 11 str . showSpace . showsPrec 11 c
            Combiner s c -> showString "Combiner " . showsPrec 11 s . showSpace . showsPrec 11 c
            Separator s -> showString "Separator " . showsPrec 11 s
            ChainDot -> showString "ChainDot"
            SyntheticDot -> showString "SyntheticDot"
            StartIndent -> showString "StartIndent"
            Comment -> showString "Comment"
            UnknownSpace -> showString "UnknownSpace"
            UnknownNewline -> showString "UnknownNewline"
            UnknownDot -> showString "UnknownDot"
            UnknownColon -> showString "UnknownColon"

instance Show (Combiner phase) where
    show Paren = "Paren"
    show Brack = "Brack"
    show Brace = "Brace"
    show Indent = "Indent"

instance Show (Separator phase) where
    show Comma = "Comma"
    show Dot = "Dot"
    show Ellipsis = "Ellipsis"
    show Semicolon = "Semicolon"
    show Colon = "Colon"
    show Space = "Space"
    show Newline = "Newline"
