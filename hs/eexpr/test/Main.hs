{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Eexpr.Text (parse)

import Data.ByteString (ByteString)
import Data.Eexpr.Mixfix (Definition(..),Associativity(..),TemplElem(..))
import Data.Eexpr.Mixfix (Table(..))
import Test.Tasty (defaultMain, testGroup, TestName, TestTree)
import Test.Tasty.HUnit ((@=?))

import qualified Data.Eexpr.Mixfix as Mixfix
import qualified Data.Eexpr.Mixfix.Grammar as Mixfix
import qualified Data.Eexpr.Types as Eexpr
import qualified Data.Set as Set
import qualified Test.Tasty.HUnit as U


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "mixfixes"
  [ testGroup "definition"
    [ testMixDef "mixfix-spec-add" specAdd defAdd
    , testMixDef "mixfix-spec-mul" specMul defMul
    , testMixDef "mixfix-spec-letin" specLetIn defLetIn
    , testMixDef "mixfix-spec-sub" specSub defSub
    ]
  , testGroup "tabulation"
    [ U.testCase "table-1" $ case Mixfix.tabulate [defMul, defAdd, defLetIn, defSub] of
      ([], Just table) -> table1 @=? table
      (warns, Just _) -> fail $ "tabulation warnings: " ++ show warns
      (warns, Nothing) -> fail $ "tabulation errors: " ++ show warns
    ]
  , testGroup "rewriting" []
  ]

------------------------------------ Helpers ------------------------------------

testMixDef :: TestName -> ByteString -> Definition () -> TestTree
testMixDef name inp outp = U.testCase name $
  case parse inp of
    ([], Right [eexpr]) -> case Mixfix.recognize eexpr of
      ([], Just def) ->
        outp @=? Mixfix.mapAnnotation (const ()) def
      (errs, Nothing) -> do
        fail $ "recognizer errors: " ++ show (Eexpr.mapAnnotation (const ()) eexpr) ++ "\n" ++ show errs
      (warns, Just _) -> do
        fail $ "recognizer warnings: " ++ show warns
    (warns, Left errs) -> fail $ "parse errors: " ++ show errs ++ "\nwarnings: " ++ show warns
    (warns, _) -> fail $ "parse warnings: " ++ show warns

------------------------------------ Test Inputs/Outputs ------------------------------------

table1 :: Table ()
table1 = Table
  [ [defLetIn]
  , [defAdd, defSub]
  , [defMul]
  ]

specAdd :: ByteString
specAdd =
  "mixfix add:\n\
  \  pattern: () + ()\n\
  \  after: mul\n\
  \  assoc: left\n\
  \"
defAdd :: Definition ()
defAdd =
  let (Right template) = Mixfix.toTemplate [Hole, Literal "+", Hole]
  in MixfixDef
    { annotation = ()
    , name = "add"
    , lowerPrecedenceThan = Set.singleton "mul"
    , samePrecedenceAs = Set.empty
    , higherPrecedenceThan = Set.empty
    , associativity = LeftAssociative
    , template
    }

specMul :: ByteString
specMul =
  "mixfix mul:\n\
  \  pattern: () * ()\n\
  \  assoc: left\n\
  \"
defMul :: Definition ()
defMul =
  let Right template = Mixfix.toTemplate [Hole, Literal "*", Hole]
  in MixfixDef
    { annotation = ()
    , name = "mul"
    , lowerPrecedenceThan = Set.empty
    , samePrecedenceAs = Set.empty
    , higherPrecedenceThan = Set.empty
    , associativity = LeftAssociative
    , template
    }

specLetIn :: ByteString
specLetIn =
  "mixfix let:\n\
  \  pattern: let () in ()\n\
  \  after: mul, add\n\
  \  assoc: right\n\
  \"
defLetIn :: Definition ()
defLetIn =
  let Right template = Mixfix.toTemplate [Literal "let", Hole, Literal "in", Hole]
   in MixfixDef
      { annotation = ()
      , name = "let"
      , lowerPrecedenceThan = Set.fromList ["add", "mul"]
      , samePrecedenceAs = Set.empty
      , higherPrecedenceThan = Set.empty
      , associativity = RightAssociative
      , template
      }

specSub :: ByteString
specSub =
  "mixfix sub:\n\
  \  pattern: () - ()\n\
  \  simul: add\n\
  \  assoc: left\n\
  \"
defSub :: Definition ()
defSub =
  let (Right template) = Mixfix.toTemplate [Hole, Literal "-", Hole]
  in MixfixDef
    { annotation = ()
    , name = "sub"
    , lowerPrecedenceThan = Set.empty
    , samePrecedenceAs = Set.fromList ["add"]
    , higherPrecedenceThan = Set.empty
    , associativity = LeftAssociative
    , template
    }
