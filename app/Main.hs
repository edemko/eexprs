{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad (forM_,when)
import Language.EExpr.Text.Lexer.Stream (mkStream,tok)
import Language.EExpr.Text.Lexer.Types (Lexeme(..))
import Language.EExpr.Text.PostLexer (contextualize)
import Language.EExpr.Text.Render.SExpr (ppReaderMacros)
import System.Environment (getArgs)
import System.Exit (exitFailure)

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Language.EExpr.Text.Lexer as Lexer
import qualified Language.EExpr.Text.Parser as Parser


main :: IO ()
main = do
  infile <- getArgs >>= pure . \case
    (infile:_) -> infile
    [] -> "testfile.in"

  testfile <- T.readFile infile
  let broad = Lexer.parse testfile
  -- FIXME filename to .raw.re
  writeFile "testfile.broad.orig" . T.unpack $ T.concat (orig <$> broad)
  when False $ mapM_ print $ (\x -> (orig x, payload x)) <$> broad -- DEBUG

  let narrow = contextualize broad
  -- FIXME filename to .contextual.re
  writeFile "testfile.narrow.orig" . T.unpack $ T.concat (orig <$> narrow)
  -- forM_ narrow $ \x -> case payload x of
  --     Ok t -> putStr "[OK ] " >> print (orig x, t)
  --     Ignore t -> putStr "[ign] " >> print (orig x, t)
  --     Error err -> putStr "[ERR] " >> print (orig x, err) -- putStrLn $ concat ["(", show (orig x), ",<error>)"]

  -- FIXME write this html stuff properly
  -- let html = Html.prepareToken infile <$> narrow
  -- writeFile "testfile.html" . intercalate "\n" $ show <$> html

  tokStream <- case mkStream (Just infile) narrow of
    Right it -> pure it
    Left (errs, _) -> do
      putStrLn $ case length errs of
        1 -> "Lexer error:"
        len -> "Lexer errors (" ++ show len ++ "):"
      forM_ errs print
      exitFailure
  when False $ mapM_ (print . tok) tokStream -- DEBUG

  errfulExprs <- case Parser.parse tokStream of
    Left err -> do
      putStrLn "Parse error:"
      print err
      exitFailure
    Right expr -> pure expr
  when True $ mapM_ (T.putStrLn . ppReaderMacros) errfulExprs -- DEBUG
