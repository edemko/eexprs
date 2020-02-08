{-# LANGUAGE LambdaCase #-}

module Main where

import Data.List(intercalate)
import Text.Nest.Tokens.Types (orig, payload)
import Data.Either (partitionEithers)
import Text.Nest.Tokens.Parse (parseFile)
import Text.Nest.Tokens.Megaparsec.Narrow (narrowParse)
import Text.Nest.Tokens.Types.Narrow (Outcome(..))
import System.Exit (exitSuccess, exitFailure)
import Control.Monad (forM_)

import qualified Data.Text as T

main :: IO ()
main = do
    broad <- parseFile "testfile.in" >>= \case
        Left err -> putStr err >> exitFailure
        Right val -> pure val
    writeFile "testfile.broad.orig" . T.unpack $ T.concat (orig <$> broad)
    -- mapM_ print $ (\x -> (orig x, payload x)) <$> broad
    let narrow = narrowParse (fmap Right <$> broad)
    writeFile "testfile.narrow.orig" . T.unpack $ T.concat (orig <$> narrow)
    forM_ narrow $ \x -> case payload x of
        Ok t -> print (orig x, t)
        Ignore _ -> pure ()
        Error err -> putStrLn $ concat ["(", show (orig x), ",<error>)"]


    -- recognized <- case partitionEithers (recognize <$> raw) of
    --     ([], it) -> pure it
    --     (errs, _) -> do
    --         putStrLn $ case length errs of
    --             1 -> "Lexer error:"
    --             len -> "Lexer errors (" ++ show len ++ "):"
    --         putStr (intercalate "\n" errs)
    --         exitFailure
    -- simple <- case partitionEithers (simplify recognized) of
    --     ([], it) -> pure it
    --     (errs, _) -> do
    --         putStrLn $ case length errs of
    --             1 -> "Lexer error:"
    --             len -> "Lexer errors (" ++ show len ++ "):"
    --         putStr (intercalate "\n" errs)
    --         exitFailure
    -- mapM_ (print . payload) simple
