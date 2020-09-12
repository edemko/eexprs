{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Text.Nest.Tokens.Lexer.Narrow
    ( narrowParse
    ) where

import Text.Nest.Tokens.Types

import Data.Bifunctor (first)
import Text.Nest.Tokens.Types.Broad (Payload(..), Result, Outcome(..))
import Text.Nest.Tokens.Lexer.Recognize (recognizeAtom, recognizeSeparator, recognizeDepth)
import Control.Window.List (window2)

import qualified Text.Nest.Tokens.Types.Broad as Broad

narrowParse :: [Result] -> [Result]
narrowParse = fst . unParse (drain step)


-- windowStep :: Result -> Result -> Maybe [Result]
-- windowStep Broad.UnknownAtom y -> [recognizeAtom (payload <$ t), y]

step :: Parse [Result]
step = pop >>= \case
    Nothing -> pure []
    Just t@LR{payload = Left err} -> pure [Error err <$ t]
    Just t@LR{payload = Right payload} -> case payload of
        Broad.UnknownAtom -> pure [recognizeAtom (payload <$ t)]
        Broad.String a b c -> pure [Ok (String a b c) <$ t]
        Broad.Bracket a b c -> pure [Ok (Bracket a b c) <$ t]
        Broad.UnknownSeparator -> pure [recognizeSeparator (payload <$ t)]
        Broad.Newline -> afterNewline (payload <$ t) []
        Broad.Whitespace -> afterSpace (payload <$ t) []
        Broad.Comment -> pure [Ignore payload <$ t]

afterSpace :: LexResult Broad.Payload -> [Result] -> Parse [Result]
afterSpace ws buf = pop >>= \case
    Nothing -> ignore
    Just t@LR{payload = Left err} -> afterSpace ws (buf `snoc` (Error err <$ t))
    Just t@LR{payload = Right payload} -> case payload of
        Broad.UnknownAtom -> push t >> commit
        Broad.String _ _ _ -> push t >> commit
        Broad.Bracket _ _ _ -> push t >> commit
        Broad.UnknownSeparator -> push t >> commit
        Broad.Newline -> push t >> ignore
        Broad.Whitespace -> whiteSpaceAfterWhitespace
        Broad.Comment -> push t >> ignore
    where
    commit = pure $ (Ok Space <$ ws) : buf
    ignore = pure $ (Ignore <$> ws) : buf

afterNewline :: LexResult Broad.Payload -> [Result] -> Parse [Result]
afterNewline nl buf = pop >>= \case
    Nothing -> ignore
    Just t@LR{payload = Left err} -> afterNewline nl (buf `snoc` (Error err <$ t))
    Just t@LR{payload = Right payload} -> case payload of
        Broad.UnknownAtom -> push t >> commit (loc t)
        Broad.String _ _ _ -> push t >> commit (loc t)
        Broad.Bracket _ _ _ -> push t >> commit (loc t)
        Broad.UnknownSeparator -> push t >> commit (loc t)
        Broad.Newline -> push t >> ignore
        Broad.Whitespace -> afterIndent ((Ignore <$> nl) : buf) (payload <$ t) []
        Broad.Comment -> push t >> ignore
    where
    commit loc = pure $ [Ignore <$> nl] ++ buf ++ [Ok <$> invented]
        where invented = LR{ loc, orig = "", payload = Indent 0 }
    ignore = pure $ (Ignore <$> nl) : buf

afterIndent :: [Result] -> LexResult Broad.Payload -> [Result] -> Parse [Result]
afterIndent nl ws buf = pop >>= \case
    Nothing -> ignore
    Just t@LR{payload = Left err} -> afterIndent nl ws (buf `snoc` (Error err <$ t))
    Just t@LR{payload = Right payload} -> case payload of
        Broad.UnknownAtom -> push t >> commit
        Broad.String _ _ _ -> push t >> commit
        Broad.Bracket _ _ _ -> push t >> commit
        Broad.UnknownSeparator -> push t >> commit
        Broad.Newline -> push t >> ignore
        Broad.Whitespace -> whiteSpaceAfterWhitespace
        Broad.Comment -> push t >> ignore
    where
    commit = pure $ nl ++ [recognizeDepth ws] ++ buf
    ignore = pure $ nl ++ [Ignore <$> ws] ++ buf


------------ Monad ------------

newtype Parse a = Parse { unParse :: [Result] -> (a, [Result]) }


pop :: Parse (Maybe Result)
pop = Parse $ \case
    [] -> (Nothing, [])
    x:xs -> (Just x, xs)

push :: Result -> Parse ()
push x = Parse $ \xs -> ((), x:xs)

drain :: Semigroup a => Parse a -> Parse a
drain (Parse action) = Parse $ (,[]) . loop
    where
    loop inp = case action inp of
        (x, []) -> x
        (x, inp') -> x <> loop inp'

whiteSpaceAfterWhitespace :: Parse a
whiteSpaceAfterWhitespace = error "Internal error: should not have seen whitespace after whitespace. Please report."


instance Functor Parse where
    fmap f k = Parse $ first f . unParse k

instance Applicative Parse where
    pure x = Parse $ (x,)
    (Parse k1) <*> (Parse k2) = Parse $ \inp ->
        let (f, inp') = k1 inp
            (x, inp'') = k2 inp'
        in (f x, inp'')

instance Monad Parse where
    return = pure
    (Parse k1) >>= k = Parse $ \inp ->
        let (x, inp') = k1 inp
            (Parse k2) = k x
        in k2 inp'


snoc :: [a] -> a -> [a]
snoc xs x = xs ++ [x]
