{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.EExpr.Text.Render.SExpr
  ( ppReaderMacros
  ) where

import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)
import Language.EExpr.Types (EExpr(..), Atom(..), Combiner(..))
import Language.EExpr.Types (NonEmpty2(..))

import qualified Data.Text as T


-- FIXME pretty-print
ppReaderMacros :: EExpr -> Text
ppReaderMacros (Atom _ atom) = case atom of
  IntAtom str _ -> str
  RatAtom str _ -> str
  SymAtom x -> x
ppReaderMacros (Combine _ StrTempl (str, [])) = T.pack (show str)
ppReaderMacros (Combine _ StrTempl (str0, splices)) = T.concat
  [ "#\"("
  , T.intercalate " " $ (T.pack . show) str0 : concat [[ppReaderMacros x, (T.pack . show) str] | (x, str) <- splices]
  , ")"
  ]
ppReaderMacros (Combine _ Paren inner) = T.concat
  [ "#round("
  , maybe "" ppReaderMacros inner
  , ")"
  ]
ppReaderMacros (Combine _ Bracket inner) = T.concat
  [ "#square("
  , maybe "" ppReaderMacros inner
  , ")"
  ]
ppReaderMacros (Combine _ Brace inner) = T.concat
  [ "#curly("
  , maybe "" ppReaderMacros inner
  , ")"
  ]
ppReaderMacros (Combine _ Indent (x :| xs)) = T.concat
  [ "#block("
  , T.intercalate " " $ ppReaderMacros <$> (x:xs)
  , ")"
  ]
ppReaderMacros (Combine _ Apply (x:|xs)) = T.concat
  [ "("
  , T.intercalate " " $ ppReaderMacros <$> (x:xs)
  , ")"
  ]
ppReaderMacros (Combine _ Ellipsis (left, right)) = T.concat
  [ "#..("
  , maybe "#nil" ppReaderMacros left
  , " "
  , maybe "#nil" ppReaderMacros right
  , ")"
  ]
ppReaderMacros (Combine _ Colon (left, right)) = T.concat
  [ "#:("
  , ppReaderMacros left
  , " "
  , ppReaderMacros right
  , ")"
  ]
ppReaderMacros (Combine _ Comma xs) = T.concat
  [ "#,("
  , T.intercalate " " $ ppReaderMacros <$> xs
  , ")"
  ]
ppReaderMacros (Combine _ Semicolon xs) = T.concat
  [ "#;("
  , T.intercalate " " $ ppReaderMacros <$> xs
  , ")"
  ]
ppReaderMacros (Combine _ Chain ((x1, x2) :|| xs)) = T.concat
  [ "#.("
  , T.intercalate " " $ ppReaderMacros <$> (x1:x2:xs)
  , ")"
  ]
ppReaderMacros (Combine _ SyntheticDot x) = T.concat
  [ "#synth("
  , ppReaderMacros x
  , ")"
  ]
ppReaderMacros (Combine _ Mixfix (op, args)) = T.concat
  [ "#mixfix("
  , T.intercalate " " $ ppReaderMacros <$> (op:args)
  , ")"
  ]
