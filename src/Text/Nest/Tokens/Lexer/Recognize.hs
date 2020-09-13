{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Nest.Tokens.Lexer.Recognize
    ( isSymbolChar
    , recognizeAtom
    , recognizeSeparator
    , recognizeDepth
    ) where

import Text.Nest.Tokens.Types

import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Text (Text)
import Text.Lightyear (Lightyear, Consume(..), Branch(..), TextPos)
import Text.Nest.Tokens.Lexer.Error (expect, crammedTokens, mixedIndent, panic)

import qualified Data.List.NonEmpty as NE
import qualified Data.Char as C
import qualified Data.Text as T
import qualified Text.Lightyear as P


-- for more options, peek around starting at https://www.compart.com/en/unicode/category
isSymbolChar :: Char -> Bool
isSymbolChar c = good && defensive
    where
    defensive = c `notElem` ("\\# \t\n\r()[]{},.;:`\'\"" :: [Char])
    good = C.isLetter c || C.isDigit c || nonModifyingSymbol || c `elem` ("~!@$%^&*-_=+|<>/?" :: [Char])
    nonModifyingSymbol = case C.generalCategory c of
        C.MathSymbol -> True
        C.CurrencySymbol -> True
        _ -> False

recognizeAtom :: TextPos -> Text -> Outcome
recognizeAtom loc orig = case P.runLightyearPos parseAtom orig loc () of
    Right a -> Ok $ Atom a
    Left err -> Error err

recognizeSeparator :: TextPos -> Text -> Outcome
recognizeSeparator loc orig = case P.runLightyearPos parseSeparator orig loc () of
    Right Dot -> Ok SensitiveDot
    Right Colon -> Ok SensitiveColon
    Right sep -> Ok $ Separator sep
    Left err -> Error err

recognizeDepth :: TextPos -> Text -> Either LexError Int
recognizeDepth loc orig = P.runLightyearPos parseDepth orig loc ()


parseAtom :: Parser 'Greedy Atom
parseAtom = do
    it <- atom
    P.endOfInput crammedTokens
    pure it

parseSeparator :: Parser 'Greedy Separator
parseSeparator = do
    it <- separator
    P.endOfInput crammedTokens
    pure it

parseDepth :: Parser 'Greedy Int
parseDepth = do
    spaces <- T.concat <$> P.many (simple <|> continue)
    P.endOfInput mixedIndent
    pure $ T.length spaces
    where
    simple = P.takeWhile1 (panic "spaces") (==' ')
    continue = "" <$ P.string (panic "line continue") "\\\n"




type Parser c a = Lightyear c () Text LexError a

atom :: Parser 'Greedy Atom
atom = do
    parser <- (parseNum <$ isNum) <|> (parseSymbol <$ isSymbol)
    parser
    where
    isNum = P.lookAhead $ P.try $ do
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

separator :: Parser 'Greedy Separator
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
