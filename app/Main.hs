{-# LANGUAGE LambdaCase #-}

module Main where

import Data.List(intercalate)
import Text.Nest.Tokens (payload)
import Data.Either (partitionEithers)
import Text.Nest.Tokens.Parse (parseFile, recognize, simplify)

import System.Exit (exitSuccess, exitFailure)

main :: IO ()
main = do
    raw <- parseFile "testfile" >>= \case
        Left err -> putStr err >> exitFailure
        Right val -> pure val
    recognized <- case partitionEithers (recognize <$> raw) of
        ([], it) -> pure it
        (errs, _) -> do
            putStrLn $ case length errs of
                1 -> "Lexer error:"
                len -> "Lexer errors (" ++ show len ++ "):"
            putStr (intercalate "\n" errs)
            exitFailure
    simple <- case partitionEithers (simplify recognized) of
        ([], it) -> pure it
        (errs, _) -> do
            putStrLn $ case length errs of
                1 -> "Lexer error:"
                len -> "Lexer errors (" ++ show len ++ "):"
            putStr (intercalate "\n" errs)
            exitFailure
    mapM_ (print . payload) simple
