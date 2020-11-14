{-# LANGUAGE NamedFieldPuns #-}

module Text.Nest.Tokens.Html
  ( RawLexInfo
  , prepareToken
  ) where

import Data.Text (Text)
import Text.Lightyear.Stream (advance)
import Text.Nest.Tokens.Types (LexResult(..), Location(..), Atom(..), StrTemplJoin(..))

import qualified Text.Nest.Tokens.Types.Broad as Broad
import qualified Text.Nest.Tokens.Types.Narrow as Narrow


data RawLexInfo = RawLexInfo
  { text :: Text
  , location :: Location
  , category :: [String]
  }
  deriving (Show)

prepareToken :: FilePath -> LexResult Narrow.Outcome -> RawLexInfo
prepareToken file LR{loc,orig,payload} = RawLexInfo
  { text=orig
  , location=Loc{file,from=loc,to=advance orig loc}
  , category=getCategory payload
  }
  where
  getCategory (Narrow.Ok narrow) = case narrow of
    Narrow.Atom (IntAtom _) -> ["atom", "numerical", "int"]
    Narrow.Atom (RatAtom _) -> ["atom", "numerical", "int"]
    Narrow.Atom (SymAtom _) -> ["atom", "name"]
    Narrow.String Plain _ Plain -> ["atom", "textual", "string"]
    Narrow.String _ _ _ -> ["atom", "textual", "splice"]
    Narrow.Bracket _ _ _ -> ["punctuation", "bracketing"]
    Narrow.Separator _ -> ["punctuation", "separator"]
    Narrow.Space -> ["whitespace", "inline"]
    Narrow.Indent _ -> ["whitespace", "indentation"]
  getCategory (Narrow.Ignore broad) = case broad of
    Broad.UnknownAtom -> ["atom"]
    Broad.String _ _ _ -> ["atom"]
    Broad.Bracket _ _ _ -> ["punctuation", "bracketing"]
    Broad.UnknownSeparator -> ["punctuation", "separator"]
    Broad.Newline -> ["whitespace", "newline"]
    Broad.Whitespace -> ["whitespace", "inline"]
    Broad.Comment -> ["comment", "line"]
  getCategory (Narrow.Error _) = ["invalid", "illegal"]

-- renderHtml :: RawLexInfo Html -- TODO
