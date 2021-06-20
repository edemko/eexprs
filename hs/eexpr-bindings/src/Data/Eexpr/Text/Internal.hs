{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Eexpr.Text.Internal
  ( ParserObj
  , newDefaultParser
  , parse
  , drainEexprs
  -- TODO drainErrors
  -- TODO drainWarnings
  -- TODO drainTokens
  , nErrors -- TODO deleteme
  ) where

import Data.Eexpr.Types
import Prelude hiding (significand)

import Control.Monad (forM_,foldM)
import Control.Monad.Primitive (PrimMonad, PrimState, unsafeIOToPrim)
import Data.Bits (shiftL, (.|.))
import Data.ByteString (ByteString)
import Data.Eexpr.Text.Ffi (CEexpr,CLocation)
import Data.Foldable (toList)
import Data.Primitive.Array (Array,newArray,writeArray,unsafeFreezeArray,arrayFromListN)
import Data.Primitive.ByteArray (ByteArray(..),newByteArray,writeByteArray,unsafeFreezeByteArray)
import Data.Text.Short (ShortText)
import Data.Word (Word8,Word32)
import Foreign.C.Types (CSize,CBool)
import Foreign.ForeignPtr (mallocForeignPtr,mallocForeignPtrBytes,withForeignPtr)
import Foreign.Ptr (Ptr,nullPtr,castPtr)
import Foreign.Storable (Storable(..))

import qualified Data.ByteString.Short.Internal as SBS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.Eexpr.Text.Ffi as Ffi
import qualified Data.List.NonEmpty as NE
import qualified Data.List.NonEmpty2 as NE2
import qualified Data.Primitive.ByteArray as Prim
import qualified Data.Text.Short.Unsafe as T


------------ Parser Objects ------------

data ParserObj s = ParserObj
  { unParser :: Prim.MutableByteArray s -- GC can track memory behind a mutable byte array, but not a Ptr. Since I use this field just to get 'Ptr's though, the ByteArray must be pinned
  , input :: {-# UNPACK #-} !ByteString -- I'm using a ByteString because it is pinned, and therefore I can pass it to C
  }

parserPtr :: ParserObj s -> Ptr Ffi.CParserObj
parserPtr = castPtr . Prim.mutableByteArrayContents . unParser

data PauseAt
  = PauseAfterStart
  | PauseAfterRawlex
  | PauseAfterCooklex
  | PauseAfterParse
  | DoNotPause
  deriving(Eq,Ord,Enum,Show)

newDefaultParser :: (PrimMonad m) => ByteString -> m (ParserObj (PrimState m))
{-# NOINLINE newDefaultParser #-}
newDefaultParser inp = do
  st <- Prim.newPinnedByteArray (fromIntegral Ffi.sizeofParser)
  let obj = ParserObj st inp
  unsafeIOToPrim $ Ffi.initDefault (parserPtr obj)
  pure obj

parse :: (PrimMonad m) => ParserObj (PrimState m) -> m ()
{-# NOINLINE parse #-}
parse st = unsafeIOToPrim $
  BS.unsafeUseAsCStringLen (input st) $ \(bytes, nBytes) ->
    Ffi.parse (parserPtr st) (fromIntegral nBytes) bytes

drainEexprs :: (PrimMonad m) => ParserObj (PrimState m) -> m (Array (Eexpr Location))
{-# NOINLINE drainEexprs #-}
drainEexprs st = unsafeIOToPrim $ do
  n <- Ffi.nEexprs (parserPtr st)
  if n == 0
  then pure emptyArray
  else do
    arr <- newArray (fromIntegral n) (error "uninitialized eexpr")
    forM_ [0 .. n-1] $ \i -> do
      cEexpr <- Ffi.eexprAt (parserPtr st) i
      eexpr <- fromC cEexpr
      Ffi.eexprDel cEexpr
      writeArray arr (fromIntegral i) eexpr
    Ffi.delEexprs (parserPtr st)
    unsafeFreezeArray arr


nErrors :: (PrimMonad m) => ParserObj (PrimState m) -> m CSize
{-# NOINLINE nErrors #-}
nErrors st = unsafeIOToPrim $
  Ffi.nErrors (parserPtr st)


------------ Eexprs ------------


fromC :: (PrimMonad m) => Ptr CEexpr -> m (Eexpr Location)
fromC eexpr_p = do
  location <- unsafeIOToPrim $ do
    loc_fp <- mallocForeignPtrBytes (fromIntegral Ffi.sizeofLoc)
    withForeignPtr loc_fp $ \loc_p -> do
      _ <- Ffi.eexprLocate eexpr_p loc_p
      copyCLoc loc_p
  case Ffi.eexprType eexpr_p of
    typ
      | typ == Ffi.eexprSymbol -> unsafeIOToPrim $ do
        nBytes_fp <- mallocForeignPtr
        bytes_fp <- mallocForeignPtr
        name <- withForeignPtr nBytes_fp $ \nBytes_p -> withForeignPtr bytes_fp $ \bytes_p -> do
          _ <- Ffi.asSymbol eexpr_p nBytes_p bytes_p
          liftJoin2 copyCUtf8Str (peek nBytes_p) (peek bytes_p)
        pure $ Symbol location name
      | typ == Ffi.eexprNumber -> unsafeIOToPrim $ do
        value_fp <- mallocForeignPtrBytes (fromIntegral Ffi.sizeofBignum)
        value <- withForeignPtr value_fp $ \value_p -> do
          _ <- Ffi.asNumber eexpr_p value_p
          significand <- liftJoin3 copyCBigint
                                    (Ffi.isPositive value_p)
                                    (Ffi.nBigDigits value_p)
                                    (Ffi.bigDigits value_p)
          radix <- Radix <$> Ffi.radix value_p
          fractionalExponent <- Ffi.nFracDigits value_p
          explicitExponent <- liftJoin3 copyCBigint
                                          (Ffi.isPositive_exp value_p)
                                          (Ffi.nBigDigits_exp value_p)
                                          (Ffi.bigDigits_exp value_p)
          pure $ Bignum{significand,radix,fractionalExponent,explicitExponent}
        pure $ Number location value
      | typ == Ffi.eexprString -> unsafeIOToPrim $ do
        tmpl_fp <- mallocForeignPtrBytes (fromIntegral Ffi.sizeofStrTemplate)
        withForeignPtr tmpl_fp $ \tmpl_p -> do
          _ <- Ffi.asString eexpr_p tmpl_p
          headString <- liftJoin2 copyCUtf8Str (Ffi.head_nBytes tmpl_p) (Ffi.head_utf8str tmpl_p)
          nTail <- Ffi.tail_nSubexprs tmpl_p
          tailParts <- if nTail == 0
            then pure emptyArray
            else do
              arr <- newArray (fromIntegral nTail) (error "uninitialized string template subexpr+text")
              forM_ [0 .. nTail-1] $ \(ci :: CSize) -> do
                let i = fromIntegral @CSize @Int ci
                !subexpr <- fromC =<< Ffi.tailAt_subexpr tmpl_p ci
                !text <- liftJoin2 copyCUtf8Str (Ffi.tailAt_nBytes tmpl_p ci) (Ffi.tailAt_utf8str tmpl_p ci)
                writeArray arr i (subexpr, text)
              unsafeFreezeArray arr
          pure $ String location headString (toList tailParts)
      | typ == Ffi.eexprParen -> unsafeIOToPrim $ do
        subexpr_p_fp <- mallocForeignPtr
        subexpr_p <- withForeignPtr subexpr_p_fp $ \subexpr_p_p -> do
          _ <- Ffi.asParen eexpr_p subexpr_p_p
          peek subexpr_p_p
        subexpr <- copyEexprNullable subexpr_p
        pure $ Paren location subexpr
      | typ == Ffi.eexprBrack -> unsafeIOToPrim $ do
        subexpr_p_fp <- mallocForeignPtr
        subexpr_p <- withForeignPtr subexpr_p_fp $ \subexpr_p_p -> do
          _ <- Ffi.asBrack eexpr_p subexpr_p_p
          peek subexpr_p_p
        subexpr <- copyEexprNullable subexpr_p
        pure $ Bracket location subexpr
      | typ == Ffi.eexprBrace -> unsafeIOToPrim $ do
        subexpr_p_fp <- mallocForeignPtr
        subexpr_p <- withForeignPtr subexpr_p_fp $ \subexpr_p_p -> do
          _ <- Ffi.asBrace eexpr_p subexpr_p_p
          peek subexpr_p_p
        subexpr <- copyEexprNullable subexpr_p
        pure $ Brace location subexpr
      | typ == Ffi.eexprBlock -> unsafeIOToPrim $ do
        nSubexprs_fp <- mallocForeignPtr
        subexprs_p_fp <- mallocForeignPtr
        withForeignPtr nSubexprs_fp $ \nSubexprs_p -> withForeignPtr subexprs_p_fp $ \subexprs_p_p -> do
          _ <- Ffi.asBlock eexpr_p nSubexprs_p subexprs_p_p
          nSubexprs <- peek nSubexprs_p
          subexprs_p <- peek subexprs_p_p
          subexprs <- copyCEexprArr nSubexprs subexprs_p
          pure $ Block location (NE.fromList $ toList subexprs)
      | typ == Ffi.eexprPredot -> unsafeIOToPrim $ do
        subexpr_p_fp <- mallocForeignPtr
        subexpr_p <- withForeignPtr subexpr_p_fp $ \subexpr_p_p -> do
          _ <- Ffi.asPredot eexpr_p subexpr_p_p
          peek subexpr_p_p
        !subexpr <- fromC subexpr_p
        pure $ Predot location subexpr
      | typ == Ffi.eexprChain -> unsafeIOToPrim $ do
        nSubexprs_fp <- mallocForeignPtr
        subexprs_p_fp <- mallocForeignPtr
        withForeignPtr nSubexprs_fp $ \nSubexprs_p -> withForeignPtr subexprs_p_fp $ \subexprs_p_p -> do
          _ <- Ffi.asChain eexpr_p nSubexprs_p subexprs_p_p
          nSubexprs <- peek nSubexprs_p
          subexprs_p <- peek subexprs_p_p
          subexprs <- copyCEexprArr nSubexprs subexprs_p
          pure $ Chain location (NE2.fromList $ toList subexprs)
      | typ == Ffi.eexprSpace -> unsafeIOToPrim $ do
        nSubexprs_fp <- mallocForeignPtr
        subexprs_p_fp <- mallocForeignPtr
        withForeignPtr nSubexprs_fp $ \nSubexprs_p -> withForeignPtr subexprs_p_fp $ \subexprs_p_p -> do
          _ <- Ffi.asSpace eexpr_p nSubexprs_p subexprs_p_p
          nSubexprs <- peek nSubexprs_p
          subexprs_p <- peek subexprs_p_p
          subexprs <- copyCEexprArr nSubexprs subexprs_p
          pure $ Space location (NE2.fromList $ toList subexprs)
      | typ == Ffi.eexprEllipsis -> unsafeIOToPrim $ do
        before_p_fp <- mallocForeignPtr
        after_p_fp <- mallocForeignPtr
        withForeignPtr before_p_fp $ \before_p_p -> withForeignPtr after_p_fp $ \after_p_p -> do
          _ <- Ffi.asEllipsis eexpr_p before_p_p after_p_p
          before <- copyEexprNullable =<< peek before_p_p
          after <- copyEexprNullable =<< peek after_p_p
          pure $ Ellipsis location before after
      | typ == Ffi.eexprColon -> unsafeIOToPrim $ do
        before_p_fp <- mallocForeignPtr
        after_p_fp <- mallocForeignPtr
        withForeignPtr before_p_fp $ \before_p_p -> withForeignPtr after_p_fp $ \after_p_p -> do
          _ <- Ffi.asColon eexpr_p before_p_p after_p_p
          before <- fromC =<< peek before_p_p
          after <- fromC =<< peek after_p_p
          pure $ Colon location before after
      | typ == Ffi.eexprComma -> unsafeIOToPrim $ do
        nSubexprs_fp <- mallocForeignPtr
        subexprs_p_fp <- mallocForeignPtr
        withForeignPtr nSubexprs_fp $ \nSubexprs_p -> withForeignPtr subexprs_p_fp $ \subexprs_p_p -> do
          _ <- Ffi.asComma eexpr_p nSubexprs_p subexprs_p_p
          nSubexprs <- peek nSubexprs_p
          subexprs_p <- peek subexprs_p_p
          subexprs <- copyCEexprArr nSubexprs subexprs_p
          pure $ Comma location (toList subexprs)
      | typ == Ffi.eexprSemicolon -> unsafeIOToPrim $ do
        nSubexprs_fp <- mallocForeignPtr
        subexprs_p_fp <- mallocForeignPtr
        withForeignPtr nSubexprs_fp $ \nSubexprs_p -> withForeignPtr subexprs_p_fp $ \subexprs_p_p -> do
          _ <- Ffi.asSemicolon eexpr_p nSubexprs_p subexprs_p_p
          nSubexprs <- peek nSubexprs_p
          subexprs_p <- peek subexprs_p_p
          subexprs <- copyCEexprArr nSubexprs subexprs_p
          pure $ Semicolon location (toList subexprs)
      | otherwise -> error "unrecognized C `eexpr_type` from C `eexpr*`"

copyCEexprArr :: CSize -> Ptr (Ptr CEexpr) -> IO (Array (Eexpr Location))
copyCEexprArr (fromIntegral -> nSubexprs) subexprs_p
  | nSubexprs == 0 = pure emptyArray
  | otherwise = do
    arr <- newArray nSubexprs (error "uninitialized eexpr")
    forM_ [0 .. nSubexprs-1] $ \i -> do
      !e <- fromC =<< peekElemOff subexprs_p i
      writeArray arr i e
    unsafeFreezeArray arr

copyCUtf8Str :: CSize -> Ptr Word8 -> IO ShortText
copyCUtf8Str (fromIntegral -> nBytes) bytes_p = do
  bytes <- newByteArray (fromIntegral nBytes)
  forM_ [0 .. nBytes-1] $ \i -> writeByteArray bytes i =<< peekElemOff bytes_p i
  (ByteArray bytes#) <- unsafeFreezeByteArray bytes
  pure $ T.fromShortByteStringUnsafe $ SBS.SBS bytes#

copyCBigint :: CBool -> CSize -> Ptr Word32 -> IO Integer
copyCBigint isPositive nBigDigits bigDigits = do
  !magnitude <- (\go -> foldM go 0 [0 .. (fromIntegral nBigDigits)-1]) $ \acc i -> do
    bigDigit <- fromIntegral @Word32 @Integer <$> peekElemOff bigDigits i
    pure $ acc .|. (bigDigit  `shiftL` (i * 32))
  pure $ if (fromIntegral @CBool @Word8 isPositive == 0 :: Bool)
    then negate magnitude
    else magnitude

copyCLoc :: Ptr CLocation -> IO Location
copyCLoc loc_p = do
  start <- do
    !byteOff <- fromIntegral <$> Ffi.locStartByte loc_p
    !lineOff <- fromIntegral <$> Ffi.locStartLine loc_p
    !colOff <- fromIntegral <$> Ffi.locStartCol loc_p
    pure LocPoint{byteOff,lineOff,colOff}
  end <- do
    !byteOff <- fromIntegral <$> Ffi.locEndByte loc_p
    !lineOff <- fromIntegral <$> Ffi.locEndLine loc_p
    !colOff <- fromIntegral <$> Ffi.locEndCol loc_p
    pure LocPoint{byteOff,lineOff,colOff}
  pure Location{start,end}

copyEexprNullable :: Ptr CEexpr -> IO (Maybe (Eexpr Location))
copyEexprNullable subexpr_p
  | subexpr_p == castPtr nullPtr = pure Nothing
  | otherwise = do
    !e <- fromC subexpr_p
    pure $ Just e


------------ Utility ------------

liftJoin2 :: Monad m => (a -> b -> m z) -> m a -> m b -> m z
liftJoin2 f ma mb = ma >>= \a -> mb >>= f a

liftJoin3 :: Monad m => (a -> b -> c -> m z) -> m a -> m b -> m c -> m z
liftJoin3 f ma mb mc = ma >>= \a -> mb >>= \b -> mc >>= f a b

emptyArray :: Array a
emptyArray = arrayFromListN 0 []
