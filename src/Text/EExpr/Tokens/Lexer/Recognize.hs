{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- FIXME I want to delete this module
module Text.EExpr.Tokens.Lexer.Recognize
    ( recognizeDepth
    ) where

import Text.EExpr.Tokens.Types

import Data.Text (Text)
import Text.EExpr.Tokens.Lexer.Error (mixedIndent, panic)
import Text.Lightyear (Lightyear, Consume(..), Branch(..), TextPos)

import qualified Data.Text as T
import qualified Text.Lightyear as P




recognizeDepth :: TextPos -> Text -> Either Error Int
recognizeDepth loc orig = P.runLightyearPos parseDepth orig loc ()


parseDepth :: Parser 'Greedy Int
parseDepth = do
    spaces <- T.concat <$> P.many (simple <|> continue)
    P.endOfInput mixedIndent
    pure $ T.length spaces
    where
    simple = P.takeWhile1 (panic "spaces") (==' ')
    continue = "" <$ P.string (panic "line continue") "\\\n"




type Parser c a = Lightyear c () Text Error a



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
