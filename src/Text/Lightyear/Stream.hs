{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS -fno-warn-orphans #-}

-- | Exposes the 'Stream' typeclass, which is instantiated
-- for types which can be used as input streams to parsers.
--
-- This is not exported by "Text.Lightyear",
-- since, except internally, the methods of this
-- class are only used to define instances.
--
-- WARNING: This module /does/ contain orphan instances of 'Stream', though.
-- So, be sure that "Text.Lightyear" /does/ export those instances.
module Text.Lightyear.Stream
    ( Stream(..)
    ) where

import Prelude hiding (length)
import Text.Lightyear.Internal
import Text.Lightyear.Position

import Data.Bifunctor (Bifunctor(..))
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Proxy (Proxy(..))
import Data.Word (Word8)

import qualified Data.Text as T
import qualified Data.ByteString as BS


instance Stream ByteString where
    type Chr ByteString = Word8
    type Pos ByteString = BytePos
    uncons = BS.uncons
    splitN n str =
        let (prefix, suffix) = BS.splitAt n str
         in if (BS.length prefix < n)
            then Nothing
            else Just (prefix, suffix)
    splitPred = BS.span
    length = BS.length
    startPosition _ = startBytePos
    advanceOne _ _ BytePos{byte} = BytePos{byte = byte + 1}
    advance str BytePos{byte} = BytePos{byte = byte + BS.length str}

instance Stream Text where
    type Chr Text = Char
    type Pos Text = TextPos
    uncons = T.uncons
    splitN n str =
        let (prefix, suffix) = T.splitAt n str
         in if (T.length prefix < n)
            then Nothing
            else Just (prefix, suffix)
    splitPred = T.span
    length = T.length
    startPosition _ = startTextPos
    -- WARNING This may give slightly incorrect positions for Windows-style newlines in edge cases.
    -- Also, it will deinitely fail for a number of other legacy newlines, see https://en.wikipedia.org/wiki/Newline#Representation
    advanceOne _ '\n' TextPos{line} = TextPos{line = line + 1, col = 0}
    advanceOne _ _ pos@TextPos{col} = pos{col = col + 1}

instance forall a. Stream a => Stream (WithFilePath a) where
    type Chr (WithFilePath a) = Chr a
    type Pos (WithFilePath a) = FilePos (Pos a)
    uncons = (second WithFilePath <$>) . uncons . unFilePath
    splitN n = (bimap WithFilePath WithFilePath <$>) . splitN n . unFilePath
    splitPred p = bimap WithFilePath WithFilePath . splitPred p . unFilePath
    length = length . unFilePath
    startPosition _ = FilePos
        { filename = "" -- WARNING: no filename supplied
        , pos = startPosition (Proxy @a)
        }
    advanceOne _ c p@FilePos{pos} = p{pos = advanceOne (Proxy @a) c pos}
    advance str p@FilePos{pos} = p{pos = advance (unFilePath str) pos}
