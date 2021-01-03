{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}

module Text.EExpr.Tokens.Parser
  ( parse
  ) where

import Text.EExpr.Tokens.Parser.Types
import Text.EExpr.Tokens.Stream

import Data.List.NonEmpty (NonEmpty((:|)))
import Text.EExpr.Tokens.Types (Location(..))
import Text.Lightyear (Lightyear, Consume(..), MakeError)

import qualified Data.List.NonEmpty as NE
import qualified Data.Sequence as Seq
import qualified Text.EExpr.Tokens.Types as Tok
import qualified Text.Lightyear as P


type Parser c a = Lightyear c () LexStream Error a



parse :: LexStream -> Either Error [EExpr]
parse inp = case Seq.viewl inp of
  Seq.EmptyL -> Right []
  x Seq.:< _ -> P.runLightyearPos parseWorker inp (from (loc x)) ()

parseWorker :: Parser 'Greedy [EExpr]
parseWorker = do
  xs <- apply `P.sepBy` newline
  P.endOfInput (expect ["end of input"])
  pure xs


enclosed :: Parser 'Greedy EExpr
enclosed = do
  (left, encloser, parseRight) <- open
  mkInner <- case encloser of
    Tok.Paren -> do
      contents <- P.option Nothing (Just <$> semicolonSeparated)
      pure $ \l -> Combine l Paren contents
    Tok.Brack -> do
      contents <- P.option Nothing (Just <$> semicolonSeparated)
      pure $ \l -> Combine l Bracket contents
    Tok.Brace -> do
      contents <- P.option Nothing (Just <$> semicolonSeparated)
      pure $ \l -> Combine l Brace contents
    Tok.Indent -> do
      contents <- colonSeparated `sepBy1` newline -- NOTE I've chosen `colonSeparated` as the parser for each line b/c it may be useful for case expressions, dictionary expressions, &c
      pure $ \l -> Combine l Indent contents
  right <- parseRight
  pure $ mkInner (loc left){to = (to . loc) right}
  where
  sepBy1 p sep = do
    x <- p
    xs <- P.option Nothing (Just <$> sep) >>= \case
      Nothing -> pure []
      Just _ -> p `P.sepBy` sep
    pure (x :| xs)

semicolonSeparated :: Parser 'Greedy EExpr
semicolonSeparated = do
  P.option Nothing (Just <$> semicolon) >>= \case
    Nothing -> loop >>= \case
      -- no semicolons at all
      (expr1:|[], Nothing) -> pure expr1
      -- semicolons, but no leading semicolon
      (expr1:|rest, mLastSemi) ->
        let xs = expr1:rest
            l = (location expr1){to = to $ maybe (location $ last xs) loc mLastSemi}
         in pure $ Combine l Semicolon xs
    Just firstSemi -> P.option Nothing (Just <$> loop) >>= \case
      -- only a single semicolon
      Nothing -> pure $ Combine (loc firstSemi) Semicolon []
      -- semicolons, including leading semicolon
      Just (expr1:|rest, mLastSemi) ->
        let xs = expr1:rest
            l = (loc firstSemi){to = to $ maybe (location $ last xs) loc mLastSemi}
         in pure $ Combine l Semicolon xs
  where
  loop = do
    x <- commaSeparated
    P.option Nothing (Just <$> semicolon) >>= \case
      Nothing -> pure (x :| [], Nothing)
      Just nextSemi -> P.option Nothing (Just <$> loop) >>= \case
        Nothing -> pure (x :| [], Just nextSemi)
        Just (xs, lastSemi) -> pure (NE.cons x xs, lastSemi)

commaSeparated :: Parser 'Greedy EExpr
commaSeparated = do
  P.option Nothing (Just <$> comma) >>= \case
    Nothing -> loop >>= \case
      -- no commas at all
      (expr1:|[], Nothing) -> pure expr1
      -- commas, but no leading comma
      (expr1:|rest, mLastComma) ->
        let xs = expr1:rest
            l = (location expr1){to = to $ maybe (location $ last xs) loc mLastComma}
         in pure $ Combine l Comma xs
    Just firstComma -> P.option Nothing (Just <$> loop) >>= \case
      -- only a single comma
      Nothing -> pure $ Combine (loc firstComma) Comma []
      -- commas, including leading comma
      Just (expr1:|rest, mLastComma) ->
        let xs = expr1:rest
            l = (loc firstComma){to = to $ maybe (location $ last xs) loc mLastComma}
         in pure $ Combine l Comma xs
  where
  loop = do
    x <- colonSeparated
    P.option Nothing (Just <$> comma) >>= \case
      Nothing -> pure (x :| [], Nothing)
      Just nextComma -> P.option Nothing (Just <$> loop) >>= \case
        Nothing -> pure (x :| [], Just nextComma)
        Just (xs, lastComma) -> pure (NE.cons x xs, lastComma)

colonSeparated :: Parser 'Greedy EExpr
colonSeparated = do
  left <- ellipsisSeparated
  P.option Nothing (Just <$> colon) >>= \case
    Nothing -> pure left
    Just _ -> do
      right <- ellipsisSeparated
      let l = (location left){to = (to . location) right}
      pure $ Combine l Colon (left, right)

ellipsisSeparated :: Parser 'Greedy EExpr
ellipsisSeparated = P.option Nothing (Just <$> apply) >>= \case
  Just left -> P.option Nothing (Just <$> ellipsis) >>= \case
    Nothing -> pure left
    Just theDots -> rightSide (Just left) theDots
  Nothing -> ellipsis >>= rightSide Nothing
  where
  rightSide left dots = do
    right <- P.option Nothing (Just <$> apply)
    let l = (maybe (loc dots) location left){to = to (maybe (loc dots) location right)}
    pure $ Combine l Ellipsis (left, right)


apply :: Parser 'Greedy EExpr
apply = do
  x <- chain
  loop >>= \case
    [] -> pure x
    xs ->
      let l = (location x){to = (to . location) (last xs)}
       in pure $ Combine l Apply (x :| xs)
  where
  loop = P.option Nothing (Just <$> space) >>= \case
    Nothing -> pure []
    Just _ -> (:) <$> chain <*> loop

chain :: Parser 'Greedy EExpr
chain = do
  mPreDot <- P.option Nothing (Just <$> syntheticDot)
  x1 <- baseTerm
  mXs <- loop
  let theChain = case mXs of
        [] -> x1
        (x2:xs) ->
          let l = (location x1){to = (to . location) (last mXs)}
           in Combine l Chain $ (x1, x2) :|| xs
  pure $ case mPreDot of
    Nothing -> theChain
    Just preDot ->
      let l = (loc preDot){to = (to . location) (theChain)}
       in Combine l SyntheticDot theChain
  where
  loop :: Parser 'Greedy [EExpr]
  loop = P.option Nothing (Just <$> chainDot) >>= \case
    Just _ -> (:) <$> baseTerm <*> loop
    Nothing -> P.option Nothing (Just <$> enclosed) >>= \case
      -- if no dot, then try to get a (), [], {}, or indent immediately after
      Just x -> (x:) <$> loop
      Nothing -> pure []
      -- TODO if I don't find a baseTerm, then we can do some error recovery here by just ending the chain with an Error ctor

baseTerm :: Parser 'Greedy EExpr
baseTerm = enclosed P.<|> string P.<|> atom

string :: Parser 'Greedy EExpr
string = do
  (firstLoc, str0, ty) <- fromOpenStr <$> P.satisfy (expect ["string"]) isOpenStr
  case ty of
    Tok.Plain -> pure $ StrAtom firstLoc str0
    Tok.Templ -> do
      (xs, lastLoc) <- loop
      let l = firstLoc{to = to lastLoc}
      pure $ Combine l StrTempl (str0, xs)
  where
  loop = do
    x <- apply
    (nextLoc, str, ty) <- fromNextStr <$> P.satisfy (expect ["string part"]) isNextStr
    case ty of
      Tok.Plain -> pure ([(x, str)], nextLoc)
      Tok.Templ -> do
        (xs, lastLoc) <- loop
        pure ((x, str) : xs, lastLoc)
  isOpenStr Lex{tok=Tok.String Tok.Plain _ _} = True
  isOpenStr _ = False
  fromOpenStr Lex{loc,tok=Tok.String Tok.Plain str ty} = (loc, str, ty)
  fromOpenStr _ = error "fromOpenStr called when isOpenStr == False"
  isNextStr Lex{tok=Tok.String Tok.Templ _ _} = True
  isNextStr _ = False
  fromNextStr Lex{loc,tok=Tok.String Tok.Templ str ty} = (loc, str, ty)
  fromNextStr _ = error "string.fromNextStr called when isNetStr == False"


atom :: Parser 'Greedy EExpr
atom = fromAtom <$> P.satisfy (expect ["atom"]) isAtom -- FIXME is this good error reporting?
  where
  isAtom Lex{tok=Tok.Atom _} = True
  isAtom _ = False
  fromAtom Lex{loc,orig,tok=Tok.Atom a} = case a of
    Tok.IntAtom i -> Atom loc $ IntAtom orig i
    Tok.RatAtom r -> Atom loc $ RatAtom orig r
    Tok.SymAtom x -> Atom loc $ SymAtom x
  fromAtom _ = errorWithoutStackTrace "fromAtom called when isAtom == False"


------ Primitives (Single Token Parsers) ------

open :: Parser 'Greedy (LexElem, Tok.Combiner 'Tok.Sens, Parser 'Greedy LexElem)
open = fromOpen <$> P.satisfy (expect ["open paren", "open bracket", "open brace", "indent"]) isOpen
  where
  isOpen Lex{tok=Tok.Combiner Tok.Open _} = True
  isOpen _ = False
  fromOpen t@Lex{tok=Tok.Combiner Tok.Open it} = case it of
    Tok.Paren -> (t, it, closeParen)
    Tok.Brack -> (t, it, closeBracket)
    Tok.Brace -> (t, it, closeBrace)
    Tok.Indent -> (t, it, closeIndent)
  fromOpen _ = errorWithoutStackTrace "fromopen called when isOpen == False"
  closeParen = P.satisfy (expect ["close paren"]) isClose
    where
    isClose Lex{tok = Tok.Combiner Tok.Close Tok.Paren} = True
    isClose _ = False
  closeBracket = P.satisfy (expect ["close bracket"]) isClose
    where
    isClose Lex{tok = Tok.Combiner Tok.Close Tok.Brack} = True
    isClose _ = False
  closeBrace = P.satisfy (expect ["close brace"]) isClose
    where
    isClose Lex{tok = Tok.Combiner Tok.Close Tok.Brace} = True
    isClose _ = False
  closeIndent = P.satisfy (expect ["dedent"]) isClose
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

semicolon :: Parser 'Greedy LexElem
semicolon = P.satisfy (expect ["semicolon"]) isSemicolon
  where
  isSemicolon Lex{tok=Tok.Separator Tok.Semicolon} = True
  isSemicolon _ = False

comma :: Parser 'Greedy LexElem
comma = P.satisfy (expect ["comma"]) isComma
  where
  isComma Lex{tok=Tok.Separator Tok.Comma} = True
  isComma _ = False

colon :: Parser 'Greedy LexElem
colon = P.satisfy (expect ["colon"]) isColon
  where
  isColon Lex{tok=Tok.Separator Tok.Colon} = True
  isColon _ = False

ellipsis :: Parser 'Greedy LexElem
ellipsis = P.satisfy (expect ["ellipsis"]) isEllipsis
  where
  isEllipsis Lex{tok=Tok.Separator Tok.Ellipsis} = True
  isEllipsis _ = False

syntheticDot :: Parser 'Greedy LexElem
syntheticDot = P.satisfy (expect ["dot"]) isDot
  where
  isDot Lex{tok=Tok.SyntheticDot} = True
  isDot _ = False

chainDot :: Parser 'Greedy LexElem
chainDot = P.satisfy (expect ["dot"]) isDot
  where
  isDot Lex{tok=Tok.ChainDot} = True
  isDot _ = False


------ Helpers ------

-- within ::
--      (Location -> [EExpr] -> EExpr) -- create this node
--   -> Parser 'Greedy EExpr -- next level down
--   -> Parser 'Greedy LexElem -- separator
--   -> Parser 'Greedy EExpr
-- within f next sep = do
--   t1 <- next
--   P.option Nothing (Just <$> sep) >>= \case
--     Nothing -> pure t1
--     Just _ -> do
--       t2 <- next
--       ts <- P.many (sep >> next)
--       let tLast = if null ts then t2 else last ts
--           l = (location t1){to=(to . location) tLast}
--       pure $ f l (t1:t2:ts)


------ Errors ------

expect :: [String] -> MakeError st LexStream Error
expect expected st = case Seq.viewl (P.input st) of
  Seq.EmptyL -> Unexpected (P.position st) Nothing expected
  Lex{loc,tok} Seq.:< _ -> Unexpected (from loc) (Just tok) expected
