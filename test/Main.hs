{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where


import Data.Functor ((<&>))
import Language.EExpr.Text.Lexer (parse)
import Language.EExpr.Text.Lexer.Results (Result(..))
import Language.EExpr.Text.Lexer.Types (Lexeme(..))
import Language.EExpr.Text.PostLexer (contextualize)
import Test.Tasty (defaultMain,TestTree,testGroup)
import Test.Tasty.Golden (goldenVsFile,writeBinaryFile)

import qualified Data.Text as T
import qualified Data.Text.IO as T

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Golden Lexing"
  [ testGroup "big 'ol smoke test"
    [ goldenVsFile "context-free stage"
        "test/examples/bigsmoke.cfgLex.golden"
        "test/examples/bigsmoke.cfgLex.output"
        $ do
          contents <- T.readFile "test/examples/bigsmoke.input"
          let cfgLex = parse contents
              renderLines = cfgLex <&> \L{..} -> case payload of
                Ok x -> "[OK ] " ++ show x ++ "\n"
                Error err -> "[ERR] " ++ show (loc, err) ++ "\n"
          writeBinaryFile "test/examples/bigsmoke.cfgLex.output" $ concat renderLines
    , goldenVsFile "context-sensitive stage"
        "test/examples/bigsmoke.fullLex.golden"
        "test/examples/bigsmoke.fullLex.output"
        $ do
          contents <- T.readFile "test/examples/bigsmoke.input"
          let fullLex = (contextualize . parse) contents
              renderLines = fullLex <&> \L{..} -> case payload of
                Ok x -> "[OK ] " ++ show x ++ "\n"
                Ignore x -> "[ign] " ++ show x ++ "\n"
                Error err -> "[ERR] " ++ show (loc, err) ++ "\n"
          writeBinaryFile "test/examples/bigsmoke.fullLex.output" $ concat renderLines
    , goldenVsFile "reconstructible original"
      "test/examples/bigsmoke.input" -- NOTE doesn't have a distinct `.golden`, sine we're looking backwards for this test
      "test/examples/bigsmoke.reconstruction.output"
      $ do
          contents <- T.readFile "test/examples/bigsmoke.input"
          let fullLex = (contextualize . parse) contents
              reconstruction = (T.unpack . T.concat) (orig <$> fullLex)
          writeBinaryFile "test/examples/bigsmoke.reconstruction.output" reconstruction
    ]
  -- TODO builds html correctly
  ]
