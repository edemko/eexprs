{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Nest.Tokens.Parse
    ( parseFile
    , parseStdin
    , recognize
    , simplify
    ) where

import Text.Nest.Tokens

import Data.Text (Text)
import Text.Nest.Tokens.Megaparsec.Location (fromLocation)
import Text.Nest.Tokens.Megaparsec.Broad (wholeFile)
import Text.Nest.Tokens.Megaparsec.Recognize (recognizeAtom, recognizeSeparator)
import Text.Nest.Tokens.Megaparsec.Simplify (indent)

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Text.Megaparsec as P


parseFile :: FilePath -> IO (Either String [Token])
parseFile fp = parse fp <$> T.readFile fp

parseStdin :: IO (Either String [Token])
parseStdin = parse "<stdin>" <$> T.getContents

parse :: FilePath -> Text -> Either String [Token]
parse fname inp = case P.parse wholeFile fname inp of
    Right toks -> Right toks
    Left errs -> Left $ P.errorBundlePretty errs


recognize :: Token -> Either String Token
recognize (Token loc (UnknownAtom t)) = case P.runParser' recognizeAtom (stateFromToken loc t) of
    (_, Right atom) -> Right (Token loc (Atom atom))
    (_, Left errs) -> Left $ P.errorBundlePretty errs
recognize (Token loc (UnknownSeparator t)) = case P.runParser' recognizeSeparator (stateFromToken loc t) of
    (_, Right sep) -> Right (Token loc (RecognizedSeparator sep))
    (_, Left errs) -> Left $ P.errorBundlePretty errs
recognize x = Right x

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

simplify :: [Token] -> [Either String Token]
simplify [] = error "token stream should have had at least an end of file"
simplify input0@(Token loc _ : _) = go (startLocation loc) Nothing input0
    where
    go :: Location -> (Maybe Token) -> [Token] -> [Either String Token]
    -- when at the end, output a final `Indent 0`
    go lastLoc _ [] =
        [Right $ Token (endLocation lastLoc) (Indent 0)]
    -- drop `Comment`s and `EndOfFile
    go _ lastWs (Token loc EndOfFile : rest) =
        go loc lastWs rest
    go _ lastWs (Token loc Comment : rest) =
        go loc lastWs rest
    -- when a newline is found, (re)initialize candidate `Indent`
    go _ _ (tok@(Token loc UnknownNewline) : rest) =
        go loc (Just (Token loc (UnknownIndent ""))) rest
    -- when whitespace is found after a newline, generate a candidate `Indent`
    -- WARNING: this relies on whitespace not following whitespace
    go _ (Just _) (Token loc (UnknownWhitespace ws) : rest) = do
        go loc (Just (Token loc (UnknownIndent ws))) rest
    -- when whitespace is otherwise found, queue a candidate `Whitespace`
    go _ _ (Token loc (UnknownWhitespace _) : rest) =
        go loc (Just (Token loc Whitespace)) rest
    -- otherwise, drain candidate and carry on
    go _ (Just (Token loc (UnknownIndent ws))) rest =
        let it = case P.runParser' indent (stateFromToken loc ws) of
                    (_, Right tok) -> Right $ Token loc tok
                    (_, Left errs) -> Left $ P.errorBundlePretty errs
        in it : go loc Nothing rest
    go _ (Just tok) rest =
        Right tok : go loc Nothing rest
    go _ Nothing (tok@(Token loc _) : rest) =
        Right tok : go loc Nothing rest

-- TODO contextualizing or bracketing next?

-- TODO I'll need a totally different approach for hilighting syntax to mark errors
