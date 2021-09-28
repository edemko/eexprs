{-# language BangPatterns #-}

import Data.Eexpr.Text (parse)
import Data.Eexpr.Types (Eexpr,Location)
import Data.Text.Encoding (encodeUtf8)
import Test.Tasty (defaultMain,testGroup)
import Test.Tasty.Golden (goldenVsString)
import Text.Show.Pretty (ppShow)

import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB

main :: IO ()
main = defaultMain $ testGroup "sample"
  [ goldenVsString "001" "sample/001/output/location.txt" (loadExprLocation "sample/001/input.txt")
  , goldenVsString "001" "sample/001/output/plain.txt" (loadExprPlain "sample/001/input.txt")
  ]

loadExprPlain :: String -> IO LB.ByteString
loadExprPlain filename = do
  e <- loadExpr filename
  let !b = encodeUtf8 (T.pack (ppShow ((fmap.fmap) (\_ -> ()) e)))
  pure (LB.fromStrict b)

loadExprLocation :: String -> IO LB.ByteString
loadExprLocation filename = do
  e <- loadExpr filename
  let !b = encodeUtf8 (T.pack (ppShow e))
  pure (LB.fromStrict b)

loadExpr :: String -> IO [Eexpr Location]
loadExpr filename = do
  contents <- B.readFile filename
  let (warnings, e) = parse contents
  case e of
    Left errors -> do
      fail $ concat $
        [ "Failed to parse.\nWarnings:\n" ]
        ++ map (\x -> show x ++ "\n") warnings
        ++ [ "Errors:\n" ]
        ++ map (\x -> show x ++ "\n") errors
    Right r -> case warnings of
      [] -> pure r
      _ -> fail $ concat $
        [ "Parsed successfully but encountered warnings:\n" ]
        ++ map (\x -> show x ++ "\n") warnings
    
