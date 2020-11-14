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

import Data.Bifunctor (bimap)
import Text.EExpr.Tokens.Types (Location(..))
import Text.Lightyear (Lightyear, Consume(..), MakeError)

import qualified Data.Sequence as Seq
import qualified Text.EExpr.Tokens.Types as Tok
import qualified Text.Lightyear as P


type Parser c a = Lightyear c () LexStream Error a



parse :: LexStream -> Either Error [EExpr ph]
parse inp = case Seq.viewl inp of
  Seq.EmptyL -> Right []
  x Seq.:< _ -> P.runLightyearPos parseWorker inp (from (loc x)) ()

parseWorker :: Parser 'Greedy [EExpr ph]
parseWorker = do
  xs <- pairing `P.sepBy` newline -- TODO is this the right starting point?
  P.endOfInput (expect ["end of input"])
  pure xs


-- surround -- TODO nil atoms, string template
surround :: Parser 'Greedy (EExpr ph)
surround = do
  (t, brak) <- open
  case brak of
    Tok.Indent -> do
      elems <- combine `P.sepBy` newline -- TODO I've chosen `combine` b/c I figure an indent should act much the same as the file top-level. However, it might be useful for it to be a pairing (e.g. for case/match/switch exprs)
      -- FIXME elems must be non-empty
      let l = (location $ head elems){to = (to . location) $ last elems}
      finish t Indent (Separate l Newline elems)
    _ -> separate >>= finish t (fromInlineSurrounder brak)
  -- TODO some form of error recovery when the enclosers don't match
  where
  finish t brak inner = do
    t' <- close brak
    let l = (loc t){to = (to . loc) t'}
    pure $ Surround l brak inner
  fromInlineSurrounder :: Tok.Combiner 'Tok.Sens -> Surrounder 'Inline
  fromInlineSurrounder Tok.Paren = Paren
  fromInlineSurrounder Tok.Brack = Bracket
  fromInlineSurrounder Tok.Brace = Brace
  fromInlineSurrounder Tok.Indent = errorWithoutStackTrace "Internal EExpr Error! Please report.\nfromInlineSurrounder: attempt to create inline surrounder from Tok.Indent"

separate :: Parser 'Greedy (EExpr ph)
separate = do
  (leadSep, leadSepTok) <- checkSep
  x <- pairing
  theSep <- case leadSep of
    Nothing -> fst <$> P.fromAtomic (P.lookAhead (P.try checkSep))
    Just it -> pure $ Just it
  case theSep of
    Nothing -> pure x
    Just punct -> do
      xs <- P.many (separator punct >> pairing)
      trailSep <- P.option Nothing (Just <$> separator punct)
      let l0 = maybe (location x) loc leadSepTok
          l' = if | Just t' <- trailSep -> loc t'
                  | null xs -> location x
                  | otherwise -> location (last xs)
          l = l0{to = to l'}
      pure $ Separate l punct (x : xs)
  where
  checkSep = P.option (Nothing, Nothing) (bimap Just Just <$> someSeparator)

pairing :: Parser 'Greedy (EExpr ph)
pairing = do
  left <- combine
  P.option Nothing (Just <$> colon) >>= \case
    Nothing -> pure left
    Just _ -> do
      right <- combine
      let l = (location left){to = (to . location) right}
      pure $ Separate l Colon (left, right)

combine :: Parser 'Greedy (EExpr ph)
combine = within Combine chain space

chain :: Parser 'Greedy (EExpr ph)
chain = P.option Nothing (Just <$> syntheticDot) >>= \case
  -- FIXME allow chains segments not explicitly separated by dots
  Nothing -> within (\l -> Chain l Standard) baseTerm chainDot
  Just t0 -> within (\l -> Chain l{from = (from . loc) t0} Naked) baseTerm chainDot

baseTerm :: Parser 'Greedy (EExpr ph)
baseTerm = surround P.<|> atom -- TODO <|> ellipsis

atom :: Parser 'Greedy (EExpr ph)
atom = fromAtom <$> P.satisfy (expect ["atom"]) isAtom -- FIXME is this good error reporting?
  where
  isAtom Lex{tok=Tok.Atom _} = True
  isAtom Lex{tok=Tok.String Tok.Plain _ Tok.Plain} = True
  isAtom Lex{tok=Tok.Separator Tok.Ellipsis} = True
  isAtom _ = False
  fromAtom Lex{loc,orig,tok=Tok.Atom a} = case a of
    Tok.IntAtom i -> IntAtom loc orig i
    Tok.RatAtom r -> RatAtom loc orig r
    Tok.SymAtom x -> SymAtom loc x
  fromAtom Lex{loc,tok=Tok.String Tok.Plain s Tok.Plain} = StrAtom loc s
  fromAtom Lex{loc,tok=Tok.Separator Tok.Ellipsis} = Ellipsis loc
  fromAtom _ = errorWithoutStackTrace "fromAtom called when isAtom == False"


------ Primitives (Single Token Parsers) ------

open :: Parser 'Greedy (LexElem, Tok.Combiner 'Tok.Sens)
open = fromOpen <$> P.satisfy (expect ["open paren", "open bracket", "open brace", "indent"]) isOpen
  where
  isOpen Lex{tok=Tok.Combiner Tok.Open _} = True
  isOpen _ = False
  fromOpen t@Lex{tok=Tok.Combiner Tok.Open it} = (t, it)
  fromOpen _ = errorWithoutStackTrace "fromopen called when isOpen == False"

close :: Surrounder anyInline -> Parser 'Greedy LexElem
close Paren = P.satisfy (expect ["close paren"]) isClose
  where
  isClose Lex{tok = Tok.Combiner Tok.Close Tok.Paren} = True
  isClose _ = False
close Bracket = P.satisfy (expect ["close bracket"]) isClose
  where
  isClose Lex{tok = Tok.Combiner Tok.Close Tok.Brack} = True
  isClose _ = False
close Brace = P.satisfy (expect ["close brace"]) isClose
  where
  isClose Lex{tok = Tok.Combiner Tok.Close Tok.Brace} = True
  isClose _ = False
close Indent = P.satisfy (expect ["dedent"]) isClose
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

separator :: Punctuation 'Listy 'Inline -> Parser 'Greedy LexElem
separator = \case
  Comma -> P.satisfy (expect ["comma"]) (isSep Comma)
  Semicolon -> P.satisfy (expect ["semicolon"]) (isSep Semicolon)
  where
  isSep :: Punctuation 'Listy 'Inline -> LexElem -> Bool
  isSep Comma Lex{tok=Tok.Separator Tok.Comma} = True
  isSep Semicolon Lex{tok=Tok.Separator Tok.Semicolon} = True
  isSep _ _ = False

someSeparator :: Parser 'Greedy (Punctuation 'Listy 'Inline, LexElem)
someSeparator = fromSep <$> P.satisfy (expect ["comma", "semicolon"]) isSep
  where
  isSep Lex{tok=Tok.Separator Tok.Comma} = True
  isSep Lex{tok=Tok.Separator Tok.Semicolon} = True
  isSep _ = False
  fromSep t@Lex{tok=Tok.Separator Tok.Comma} = (Comma, t)
  fromSep t@Lex{tok=Tok.Separator Tok.Semicolon} = (Semicolon, t)
  fromSep _ = errorWithoutStackTrace "fromSep called when isSep == False"

colon :: Parser 'Greedy LexElem
colon = P.satisfy (expect ["colon"]) isColon
  where
  isColon Lex{tok=Tok.Separator Tok.Colon} = True
  isColon _ = False

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

within ::
     (Location -> [EExpr ph] -> EExpr ph) -- create this node
  -> Parser 'Greedy (EExpr ph) -- next level down
  -> Parser 'Greedy LexElem -- separator
  -> Parser 'Greedy (EExpr ph)
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


------ Errors ------

expect :: [String] -> MakeError st LexStream Error
expect expected st = case Seq.viewl (P.input st) of
  Seq.EmptyL -> Unexpected (P.position st) Nothing expected
  Lex{loc,tok} Seq.:< _ -> Unexpected (from loc) (Just tok) expected
