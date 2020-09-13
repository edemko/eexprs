{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad (forM_)
import Data.List (intercalate)
import System.Exit (exitFailure)
import Text.Nest.Tokens.Lexer.ContextFree (parse)
import Text.Nest.Tokens.Lexer.ContextSensitive (contextualize)
import Text.Nest.Tokens.Types (orig, payload, Result, Outcome(..), Payload, LexResult(LR), LexError)

import qualified Data.Text as T
import qualified Data.Text.IO as T
-- import qualified Text.Nest.Tokens.Html as Html


main :: IO ()
main = do
    let infile = "testfile.in"

    testfile <- T.readFile infile
    let broad = parse testfile
    -- FIXME filename to .raw.re
    writeFile "testfile.broad.orig" . T.unpack $ T.concat (orig <$> broad)
    mapM_ print $ (\x -> (orig x, payload x)) <$> broad

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

    simple <- case partitionErrors narrow of
        ([], it) -> pure it
        (errs, _) -> do
            putStrLn $ case length errs of
                1 -> "Lexer error:"
                len -> "Lexer errors (" ++ show len ++ "):"
            putStrLn (intercalate "\n" (show <$> errs))
            exitFailure
    mapM_ (print . payload) simple

partitionErrors :: [Result] -> ([LexResult LexError], [LexResult Payload])
partitionErrors = go [] []
    where
    go l r [] = (reverse l, reverse r)
    go l r (x@LR{payload=Ok tok} : xs) = go l (x{payload=tok}:r) xs
    go l r (LR{payload=Ignore _} : xs) = go l r xs
    go l r (x@LR{payload=Error err} : xs) = go (x{payload=err}:l) r xs
