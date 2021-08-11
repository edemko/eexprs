{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM_)
import Data.ByteString (ByteString)
import Data.Eexpr.Text (parse)
import Data.Eexpr.Text.Normal (normalText)
import Data.Primitive.Array (sizeofArray,indexArray)

import qualified Data.ByteString as BS
import qualified Data.Text.IO as T

input :: ByteString
input =
  "smth ( \n\
  \  do:\n\
  \    inner\n\
  \  outer\n\
  \)  second : ..\n\
  \"

main :: IO ()
main = do
  BS.putStrLn input
  putStrLn "Hello, eexpr-bindings!"
  let (warns, res) = parse input
  forM_ warns print
  case res of
    Left errs -> do
      forM_ errs print
    Right eexprs -> do
      forM_ eexprs $ \eexpr -> do
        T.putStrLn $ normalText eexpr
  putStrLn "Goodbyte, and good luck!"
