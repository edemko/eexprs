{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}

module Text.Nest.Tokens.Parser
  ( parse
  ) where

import Data.Nest.Types
import Text.Nest.Tokens.Stream

import Text.Lightyear (Lightyear, Consume(..), TextPos, MakeError)
import Text.Nest.Tokens.Types (Location(..))

import qualified Data.Sequence as Seq
import qualified Text.Lightyear as P
import qualified Text.Nest.Tokens.Types as Tok


type Parser c a = Lightyear c () LexStream Error a
type Token = Tok.Token 'Tok.Sens



parse :: LexStream -> Either Error [Nest Location]
parse inp = case Seq.viewl inp of
  Seq.EmptyL -> Right []
  x Seq.:< _ -> P.runLightyearPos parseWorker inp (from (loc x)) ()

parseWorker :: Parser 'Greedy [Nest Location]
parseWorker = do
  xs <- P.many atom
  P.endOfInput (expect ["end of input"])
  pure xs

atom :: Parser 'Greedy (Nest Location)
atom = fromAtom <$> P.satisfy (expect ["atom"]) isAtom -- FIXME is this good error reporting?
  where
  isAtom Lex{tok=Tok.Atom _} = True
  isAtom _ = False
  fromAtom Lex{loc,tok=Tok.Atom a} = case a of
    Tok.IntAtom i -> IntAtom loc i
    Tok.RatAtom r -> RatAtom loc r
    Tok.SymAtom x -> SymAtom loc x
  fromAtom _ = errorWithoutStackTrace "fromAtom called when isAtom == False"


------ Errors ------

data Error
    = Unexpected TextPos (Maybe Token) [String] -- unexpected character/end-of-input, expected set
    -- TODO
    | Panic String -- for when error conditions shouldn't get thrown
    | Bundle [Error]
    deriving (Show)

instance Semigroup Error where
    (Panic _) <> b = b
    a <> (Panic _) = a
    (Bundle as) <> (Bundle bs) = Bundle (as <> bs)
    (Bundle as) <> b = Bundle (as ++ [b])
    a <> (Bundle bs) = Bundle (a : bs)
    a <> b = Bundle [a, b]
    -- TODO when multiple `Unexpected`s occur at same location, merge expectation list

expect :: [String] -> MakeError st LexStream Error
expect expected st = case Seq.viewl (P.input st) of
  Seq.EmptyL -> Unexpected (P.position st) Nothing expected
  Lex{loc,tok} Seq.:< _ -> Unexpected (from loc) (Just tok) expected
