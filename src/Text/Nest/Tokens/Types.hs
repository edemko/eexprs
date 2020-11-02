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

A 'Lexeme' combines the raw unmodified source string, its location, and any semantics that have been extracted so far.
The type of the extracted semantics is parameterized.
Ultimately, we would like this metadata to be a 'Token', which can hold all valid semantics for tokens.
However, during lexing, we keep errors and ignored tokens in the lexeme stream to aid error reporting and recovery and syntax highlighting;
    for this we use 'Result' instead.
The 'Token' type is further indexed by the phase(s) of lexing within which it is valid:
    it can be 'Free' for the (mostly) context-free portion only,
    'Sens' for the context-sensitive portion, or
    polymorphic for either phase.
Thus, the overal progress of lexing is
    @Text -> [Lexeme (Result 'Free)] -> [Lexeme (Result 'Sens)] -> [Lexeme (Token 'Sens)]@

Lexing happens in two stages: broad and narrow.
The broad phase extracts out where token boundaries are definitely going to be,
but does not do much to categorize those tokens.
Narrow lexing analyzes each body to ensure it is a single recognizable token,
and also analyzes it in context to see if it is relevant for parsing,
or if it should take on different semantics based on nearby whitespace.
-}
module Text.Nest.Tokens.Types
    (
    -- * Token Parts
      Token(..)
    , Atom(..)
    , StrTemplJoin(..)
    , Side(..)
    , Combiner(..)
    , Separator(..)
    -- * Results
    , Phase(..)
    , Result(..)
    , Lexeme(..)
    , Error(..)
    , SomeResult(..)
    -- * Location
    , Location(..)
    , startLocation
    , endLocation
    ) where

import Data.Kind

import Data.Text (Text)
import GHC.Show (showSpace)
import Text.Lightyear.Position (TextPos(..))


------------ Token ------------

-- | Index 'Token's by the phase(s) of lexing in which they are admissible.
data Phase
    = Free -- ^ after context-free parsing
    | Sens -- ^ after context-sensitive parsing

-- | The semantics of (data carried by) an indivisible, non-overlapping unit of source code.
-- C.f. 'Lexeme'.
data Token :: Phase -> Type where
    Atom :: Atom -> Token phase
    String :: StrTemplJoin -> Text -> StrTemplJoin -> Token phase
    Combiner :: Side -> Combiner phase -> Token phase
    Separator :: Separator phase -> Token phase
    ChainDot :: Token 'Sens
    SyntheticDot :: Token 'Sens
    -- | a colon in the source code that indicates that an indented block follows
    StartIndent :: Token 'Free -- FIXME not actually allowable except in the midst of context-sensitive parsing
    Comment :: Token 'Free
    -- | inline whitespace in the source code which may (or might not) carry meaning for the adjacent tokens
    UnknownSpace :: Token 'Free
    -- | a newline in the source code which may (or might not) be relevant to indendation
    UnknownNewline :: Token 'Free
    -- | a period in the source code whose purpose is not yet discovered
    UnknownDot :: Token 'Free
    -- | a colon in the source code whose purpose is not yet discovered
    UnknownColon :: Token 'Free

-- | These were refactored out of 'Token' to reduce branches in case expressions.
data Atom
    = IntAtom Integer
    | RatAtom Rational
    | SymAtom Text
    deriving (Show)

-- | Each bit of string data in the source code might be a part of a temlate.
-- This enum determines, for just one side of the string data, whether it should be grouped with the expressions on that side as part of a template.
data StrTemplJoin
    = Plain -- ^ indicates the beginning/end of a string or string template
    | Templ -- ^ indicates the beginning/end of a splice into this string template.
    deriving(Eq, Show, Read)

data Combiner :: Phase -> Type where
    Paren :: Combiner phase
    Brack :: Combiner phase
    Brace :: Combiner phase
    Indent :: Combiner 'Sens

data Side = Open | Close
    deriving(Eq, Show, Read)

data Separator :: Phase -> Type where
    Comma :: Separator phase
    Dot :: Separator 'Sens
    Ellipsis :: Separator phase
    Semicolon :: Separator phase
    Colon :: Separator 'Sens
    Space :: Separator 'Sens
    Newline :: Separator 'Sens


------------ Results ------------


-- | Existentially quantifies over the 'Phase' index of 'Result'.
-- We need this so that we can work on a heterogenously phase-indexed 'Token' stream during context-sensitive parsing.
data SomeResult = forall phase. SO (Result phase)

-- | Packages the original source characters and their location along with the (parameterized) semantics.
-- During lexing, we only parameterize with 'Token', 'Result', or 'SomeResult'.
data Lexeme a = L
    { loc :: TextPos
    , orig :: Text
    , payload :: a
    }
    deriving(Functor,Show)

data Result :: Phase -> Type where
    Ok :: Token phase -> Result phase
    Ignore :: Token phase -> Result 'Sens
    Error :: Error -> Result phase

data Error
    = BadChar TextPos Char
    | Unexpected TextPos (Maybe Char) [String] -- unexpected character/end-of-input, expected set
    | CrammedTokens TextPos Text -- tokens after first
    | MixedIndent TextPos Char -- unexpected whitespace char
    | IllegalDot -- dot with whitespace after, but not before
    | IllegalIndent -- if file starts with indent, or indent is smaller than previous level
    | Panic String -- for when error conditions shouldn't get thrown
    | Bundle [Error]
    deriving (Show)

instance Semigroup Error where
    (Panic _) <> b = b
    a <> (Panic _) = a
    (Bundle as) <> (Bundle bs) = Bundle (as <> bs)
    (Bundle as) <> b = Bundle (as ++ [b])
    a <> (Bundle bs) = Bundle (a : bs)
    a <> b = Bundle [a, b]


------------ Location ------------

data Location = Loc
    { file :: Maybe FilePath
    , from :: TextPos
    , to :: TextPos
    }

instance Show Location where
    show loc = concat [filePart, linecolPart]
        where
        filePart = maybe "" (++": ") (file loc)
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

instance Show (Result phase) where
    showsPrec p tok = showParen (p >= 11) $
        case tok of
            Ok x -> showString "Ok " . showsPrec 11 x
            Ignore x -> showString "Ignore " . showsPrec 11 x
            Error err -> showString "Error " . showsPrec 11 err

instance Show (Token phase) where
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
