{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Nest.Tokens.Lexer.Recognize
    ( recognizeAtom
    , recognizeSeparator
    , recognizeDepth
    ) where

import Text.Nest.Tokens.Types

import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Text (Text)
import Data.Void (Void)
import Text.Lightyear (Lightyear, Consume(..), Branch(..))
import Text.Nest.Tokens.Lexer.Broad (isSymbolChar)
import Text.Nest.Tokens.Lexer.Error (expect, crammedTokens, mixedIndent, panic)
import Text.Nest.Tokens.Types (LexError(..))
import Text.Nest.Tokens.Types.Narrow (Payload(..),Result,Outcome(..))

import qualified Data.List.NonEmpty as NE
import qualified Data.Char as C
import qualified Data.Text as T
import qualified Text.Lightyear as P
import qualified Text.Nest.Tokens.Types.Broad as Broad


recognizeAtom :: LexResult Broad.Payload -> Result
recognizeAtom t@LR{loc,orig,payload = Broad.Atom} =
    case P.runLightyearPos parseAtom orig loc undefined of
        Right a -> Ok (Atom a) <$ t
        Left err -> Error err <$ t
recognizeAtom _ = error "Internal error: recognizeAtom called on non-atom. Please report."

recognizeSeparator :: LexResult Broad.Payload -> Result
recognizeSeparator t@LR{loc,orig,payload = Broad.Separator} =
    case P.runLightyearPos parseSeparator orig loc undefined of
        Right sep -> Ok (Separator sep) <$ t
        Left err -> Error err <$ t
recognizeSeparator _ = error "Internal error: recognizeSeparator called on non-separator. Please report."

recognizeDepth :: LexResult Broad.Payload -> Result
recognizeDepth t@LR{loc,orig,payload = Broad.Whitespace} =
    case P.runLightyearPos parseDepth orig loc undefined of
        Right depth -> Ok (Indent depth) <$ t
        Left err -> Error err <$ t
recognizeDepth _ = error "Internal error: recognizeDepth called on non-whitespace. Please report."


parseAtom :: Parser 'Consuming Atom
parseAtom = do
    it <- atom
    P.endOfInput crammedTokens
    pure it

parseSeparator :: Parser 'Consuming Separator
parseSeparator = do
    it <- separator
    P.endOfInput crammedTokens
    pure it

parseDepth :: Parser 'Consuming Int
parseDepth = do
    spaces <- T.concat <$> P.many (simple <|> continue)
    P.endOfInput mixedIndent
    pure $ T.length spaces
    where
    simple = P.takeWhile1 (panic "spaces") (==' ')
    continue = "" <$ P.string (panic "line continue") "\\\n"




type Parser c a = Lightyear c Void Text LexError a

atom :: Parser 'Consuming Atom
atom = do
    parser <- (parseNum <$ isNum) <|> (parseSymbol <$ isSymbol)
    parser
    where
    isNum = P.lookAhead $ do
        _ <- P.option_ $ P.satisfy (panic "isNum sign") (`elem` ['+', '-'])
        P.satisfy (panic "isNum digit") C.isDigit
    isSymbol = pure ()
    parseNum = do
        sign <- P.option 1 $ (1 <$ P.char (panic "+") '+') <|> ((-1) <$ P.char (panic "-") '-')
        -- TODO I've only impelmented decimal integers
        -- TODO allow for underscores as digit group separators
        whole <- read . T.unpack <$> P.takeWhile1 (panic "parseNum digit") C.isDigit
        pure $ IntAtom (sign * whole)
    parseSymbol = SymAtom <$> P.takeWhile1 (panic "symbol character") (\c -> isSymbolChar c || c == ':') -- WARNING is too permissive, but it doesn't matter b/c isNum is checked before isSymbol

separator :: Parser 'Consuming Separator
separator = P.choice $ NE.fromList
    [ sem <$ P.choice (parseStr <$> ss)
    | (sem, ss) <- separators
    ]
    where
    parseStr s = P.string (expect [T.unpack s]) s


separators :: [(Separator, NonEmpty Text)]
separators =
    -- WARNING: each token must come after all tokens it prefixes
    [ (Comma, "," :| [])
    , (Ellipsis, ".." :| [])
    , (Dot, "." :| [])
    , (Semicolon, ";" :| [])
    , (Colon, ":" :| [])
    ]


-- stateFromToken :: Location -> Text -> P.State Text e
-- stateFromToken loc0 t0 = P.State
--     { P.stateInput = t0
--     , P.stateOffset = 0
--     , P.statePosState = P.PosState
--         { P.pstateInput = t0
--         , P.pstateOffset = 0
--         , P.pstateSourcePos = fst (fromLocation loc0)
--         , P.pstateTabWidth = P.mkPos 8
--         , P.pstateLinePrefix = ""
--         }
--     , P.stateParseErrors = []
--     }