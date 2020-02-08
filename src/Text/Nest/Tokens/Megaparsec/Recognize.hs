{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Nest.Tokens.Megaparsec.Recognize
    ( recognizeAtom
    , recognizeSeparator
    , recognizeDepth
    ) where

import Text.Nest.Tokens.Types

import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec, (<|>), SourcePos, ParseErrorBundle)
import Text.Nest.Tokens.Megaparsec.Broad (isSymbolChar)
import Text.Nest.Tokens.Types.Narrow (Payload(..),Result,Outcome(..))
import Text.Nest.Tokens.Megaparsec.Location (fromLocation)

import qualified Data.Char as C
import qualified Data.Text as T
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Pos as P
import qualified Text.Nest.Tokens.Types.Broad as Broad


recognizeAtom :: LexResult Broad.Payload -> Result
recognizeAtom t@LR{loc,orig,payload = Broad.Atom} = case P.runParser' parseAtom (stateFromToken loc orig) of
    (_, Right atom) -> Ok (Atom atom) <$ t
    (_, Left errs) -> undefined -- FIXME Error (P.errorBundlePretty errs)
recognizeAtom t@LR{loc,payload} = error "Internal error: recognizeAtom called on non-atom. Please report."

recognizeSeparator :: LexResult Broad.Payload -> Result
recognizeSeparator t@LR{loc,orig,payload = Broad.Separator} = case P.runParser' parseSeparator (stateFromToken loc orig) of
    (_, Right sep) -> Ok (Separator sep) <$ t
    (_, Left errs) -> undefined -- FIXME Error (P.errorBundlePretty errs)
recognizeSeparator t@LR{loc,payload} = error "Internal error: recognizeSeparator called on non-separator. Please report."

recognizeDepth :: LexResult Broad.Payload -> Result
recognizeDepth t@LR{loc,orig,payload = Broad.Whitespace} = case P.runParser' parseDepth (stateFromToken loc orig) of
    (_, Right depth) -> Ok (Indent depth) <$ t
    (_, Left errs) -> undefined -- FIXME Error (P.errorBundlePretty errs)
recognizeDepth t@LR{loc,payload} = error "Internal error: recognizeDepth called on non-whitespace. Please report."

parseAtom :: Parser Atom
parseAtom = do
    it <- atom
    P.label "end of token" P.eof
    pure it

parseSeparator :: Parser Separator
parseSeparator = do
    sep <- separator
    P.label "end of token" P.eof
    pure sep

parseDepth :: Parser Int
parseDepth = do
    spaces <- T.concat <$> P.many (simple <|> continue)
    P.label "end of token" P.eof
    pure $ T.length spaces
    where
    simple = P.takeWhile1P (Just "spaces") (==' ')
    continue = "" <$ P.string "\\\n"




type Parser = Parsec Void Text

atom :: Parser Atom
atom = do
    parser <- (parseNum <$ isNum) <|> (parseSymbol <$ isSymbol)
    parser
    where
    isNum = P.lookAhead . P.try $ do
        _ <- P.optional (P.oneOf ['+', '-'])
        P.satisfy C.isDigit
    isSymbol = pure ()
    parseNum = do
        sign <- P.option 1 $ (1 <$ P.char '+') <|> ((-1) <$ P.char '-')
        -- TODO I've only impelmented decimal integers
        -- TODO allow for underscores as digit group separators
        whole <- read . T.unpack <$> P.takeWhile1P (Just "digit") C.isDigit
        pure $ IntAtom (sign * whole)
    parseSymbol = SymAtom <$> P.takeWhile1P (Just "symbol character") (\c -> isSymbolChar c || c == ':') -- WARNING is too permissive, but it doesn't matter b/c isNum is checked before isSymbol

separator :: Parser Separator
separator =
    P.choice [ P.label (show sem) $
        sem <$ P.choice (P.string <$> ss)
    | (sem, ss) <- separators
    ]


separators :: [(Separator, [Text])]
separators =
    -- WARNING: each token must come after all tokens it prefixes
    [ (Comma, [","])
    , (Ellipsis, [".."])
    , (Dot, ["."])
    , (Semicolon, [";"])
    , (Colon, [":"])
    ]


stateFromToken :: Location -> Text -> P.State Text e
stateFromToken loc0 t0 = P.State
    { P.stateInput = t0
    , P.stateOffset = 0
    , P.statePosState = P.PosState
        { P.pstateInput = t0
        , P.pstateOffset = 0
        , P.pstateSourcePos = fst (fromLocation loc0)
        , P.pstateTabWidth = P.mkPos 8
        , P.pstateLinePrefix = ""
        }
    , P.stateParseErrors = []
    }