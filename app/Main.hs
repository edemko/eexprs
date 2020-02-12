{-# LANGUAGE LambdaCase #-}

module Main where

import Text.Nest.Tokens.Types (orig, payload)
import Text.Nest.Tokens.Lexer.Broad (parse)
import Text.Nest.Tokens.Lexer.Narrow (narrowParse)
import Text.Nest.Tokens.Types.Narrow (Outcome(..))
import Control.Monad (forM_)

import qualified Data.Text as T
import qualified Data.Text.IO as T

main :: IO ()
main = do
    testfile <- T.readFile "testfile.in"
    let broad = parse testfile
    writeFile "testfile.broad.orig" . T.unpack $ T.concat (orig <$> broad)
    -- mapM_ print $ (\x -> (orig x, payload x)) <$> broad

    let narrow = narrowParse broad
    writeFile "testfile.narrow.orig" . T.unpack $ T.concat (orig <$> narrow)
    forM_ narrow $ \x -> case payload x of
        Ok t -> print (orig x, t)
        Ignore _ -> pure ()
        Error err -> print (orig x, err) -- putStrLn $ concat ["(", show (orig x), ",<error>)"]


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
