{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Text.Nest.Tokens.Parser
  ( parse
  ) where

import Text.Nest.Tokens.Parser.Types
import Text.Nest.Tokens.Stream

import Text.Lightyear (Lightyear, Consume(..), MakeError)
import Text.Nest.Tokens.Types (Location(..))

import qualified Data.Sequence as Seq
import qualified Text.Lightyear as P
import qualified Text.Nest.Tokens.Types as Tok


type Parser c a = Lightyear c () LexStream Error a



parse :: LexStream -> Either Error [Nest ph]
parse inp = case Seq.viewl inp of
  Seq.EmptyL -> Right []
  x Seq.:< _ -> P.runLightyearPos parseWorker inp (from (loc x)) ()

parseWorker :: Parser 'Greedy [Nest ph]
parseWorker = do
  xs <- combine `P.sepBy` newline -- TODO is this the right starting point?
  P.endOfInput (expect ["end of input"])
  pure xs


semicolonExprs :: Parser 'Greedy (Nest ph)
semicolonExprs = around (\l -> Separate l Semicolon) commaExprs semicolon

commaExprs :: Parser 'Greedy (Nest ph)
commaExprs = around (\l -> Separate l Comma) colonExpr comma

colonExpr :: Parser 'Greedy (Nest ph)
colonExpr = do
  left <- combine
  P.option Nothing (Just <$> colon) >>= \case
    Nothing -> pure left
    Just _ -> do
      right <- combine
      let l = (location left){to = (to . location) right}
      pure $ Colon l left right

combine :: Parser 'Greedy (Nest ph)
combine = within Combine chain space

-- FIXME prefix dot is a thing
chain :: Parser 'Greedy (Nest ph)
chain = within Chain baseTerm chainDot

baseTerm :: Parser 'Greedy (Nest ph)
baseTerm = enclosed P.<|> atom -- TODO <|> ellipsis

-- enclosed -- TODO parens, brackets, braces, indent, string template
enclosed :: Parser 'Greedy (Nest ph)
enclosed = do
  (t, encloser) <- open
  case encloser of
    Tok.Indent -> do
      inner <- combine `P.sepBy` newline -- TODO I've chosen `combine` b/c I figure an indent should act much the same as the file top-level. However, it might be useful for it to be a colonExpr (e.g. for case/match/switch exprs)
      t' <- close encloser
      -- TODO some form of error recovery when the enclosers don't match
      let l = (loc t){to = (to . loc) t'}
      pure $ Indent l inner
    _ -> do
      inside <- semicolonExprs
      t' <- close encloser
      -- TODO some form of error recovery when the enclosers don't match
      let l = (loc t){to = (to . loc) t'}
      pure $ Enclose l (fromEncloser encloser) inside
      -- TODO
      -- pure $ case inside of
      --   Nothing -> NilAtom l (fromEncloser encloser)
      --   Just inner -> Enclose l (fromEncloser encloser) inner
  where
  fromEncloser Tok.Paren = Paren
  fromEncloser Tok.Brack = Bracket
  fromEncloser Tok.Brace = Brace
  fromEncloser Tok.Indent = errorWithoutStackTrace "fromEncloser called on Tok.Indent"



atom :: Parser 'Greedy (Nest ph)
atom = fromAtom <$> P.satisfy (expect ["atom"]) isAtom -- FIXME is this good error reporting?
  where
  isAtom Lex{tok=Tok.Atom _} = True
  isAtom Lex{tok=Tok.String Tok.Plain _ Tok.Plain} = True
  isAtom _ = False
  fromAtom Lex{loc,orig,tok=Tok.Atom a} = case a of
    Tok.IntAtom i -> IntAtom loc orig i
    Tok.RatAtom r -> RatAtom loc orig r
    Tok.SymAtom x -> SymAtom loc x
  fromAtom Lex{loc,tok=Tok.String Tok.Plain s Tok.Plain} = StrAtom loc s
  fromAtom _ = errorWithoutStackTrace "fromAtom called when isAtom == False"


------ Primitives (Single Token Parsers) ------

open :: Parser 'Greedy (LexElem, Tok.Combiner 'Tok.Sens)
open = fromOpen <$> P.satisfy (expect ["open paren", "open bracket", "open brace", "indent"]) isOpen
  where
  isOpen Lex{tok=Tok.Combiner Tok.Open _} = True
  isOpen _ = False
  fromOpen t@Lex{tok=Tok.Combiner Tok.Open it} = (t, it)
  fromOpen _ = errorWithoutStackTrace "fromopen called when isOpen == False"

close :: Tok.Combiner 'Tok.Sens -> Parser 'Greedy LexElem
close Tok.Paren = P.satisfy (expect ["close paren"]) isClose
  where
  isClose Lex{tok = Tok.Combiner Tok.Close Tok.Paren} = True
  isClose _ = False
close Tok.Brack = P.satisfy (expect ["close bracket"]) isClose
  where
  isClose Lex{tok = Tok.Combiner Tok.Close Tok.Brack} = True
  isClose _ = False
close Tok.Brace = P.satisfy (expect ["close brace"]) isClose
  where
  isClose Lex{tok = Tok.Combiner Tok.Close Tok.Brace} = True
  isClose _ = False
close Tok.Indent = P.satisfy (expect ["dedent"]) isClose
  where
  isClose Lex{tok = Tok.Combiner Tok.Close Tok.Indent} = True
  isClose _ = False


newline :: Parser 'Greedy LexElem
newline = P.satisfy (expect ["newline"]) isNewline
  where
  isNewline Lex{tok=Tok.Separator Tok.Newline} = True
  isNewline _ = False

space :: Parser 'Greedy LexElem
space = P.satisfy (expect ["space"]) isSpace
  where
  isSpace Lex{tok=Tok.Separator Tok.Space} = True
  isSpace _ = False

colon :: Parser 'Greedy LexElem
colon = P.satisfy (expect ["colon"]) isColon
  where
  isColon Lex{tok=Tok.Separator Tok.Colon} = True
  isColon _ = False

semicolon :: Parser 'Greedy LexElem
semicolon = P.satisfy (expect ["semicolon"]) isSemicolon
  where
  isSemicolon Lex{tok=Tok.Separator Tok.Semicolon} = True
  isSemicolon _ = False

chainDot :: Parser 'Greedy LexElem
chainDot = P.satisfy (expect ["dot"]) isDot
  where
  isDot Lex{tok=Tok.ChainDot} = True
  isDot _ = False

comma :: Parser 'Greedy LexElem
comma = P.satisfy (expect ["comma"]) isComma
  where
  isComma Lex{tok=Tok.Separator Tok.Comma} = True
  isComma _ = False


------ Helpers ------

within ::
     (Location -> [Nest ph] -> Nest ph) -- create this node
  -> Parser 'Greedy (Nest ph) -- next level down
  -> Parser 'Greedy LexElem -- separator
  -> Parser 'Greedy (Nest ph)
within f next sep = do
  t1 <- next
  P.option Nothing (Just <$> sep) >>= \case
    Nothing -> pure t1
    Just _ -> do
      t2 <- next
      ts <- P.many (sep >> next)
      let tLast = if null ts then t2 else last ts
          l = (location t1){to=(to . location) tLast}
      pure $ f l (t1:t2:ts)

around ::
     (Location -> [Nest ph] -> Nest ph) -- create this node
  -> Parser 'Greedy (Nest ph) -- next level down
  -> Parser 'Greedy LexElem -- separator
  -> Parser 'Greedy (Nest ph)
around f next sep = do
  sep0 <- P.option Nothing (Just <$> sep)
  ts <- next `P.sepBy` sep
  sep' <- P.option Nothing (Just <$> sep)
  case (sep0, ts, sep') of
    (Nothing, [], Nothing) -> error "TODO unimplemented"
    (Just t, [], Nothing) -> pure $ f (loc t) []
    (Nothing, [], Just t) -> pure $ f (loc t) []
    (t0, _, t') ->
      let l0 = maybe (location $ head ts) loc t0
          l = l0{to = to $ maybe (location $ last ts) loc t'}
       in pure $ f l ts


------ Errors ------

expect :: [String] -> MakeError st LexStream Error
expect expected st = case Seq.viewl (P.input st) of
  Seq.EmptyL -> Unexpected (P.position st) Nothing expected
  Lex{loc,tok} Seq.:< _ -> Unexpected (from loc) (Just tok) expected
