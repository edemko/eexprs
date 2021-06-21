module Data.Eexpr.Text
  ( parse
  ) where

import Control.Monad.ST (runST)
import Data.ByteString (ByteString)
import Data.Eexpr.Types (Eexpr,Error,Location)
import Data.Foldable (toList)
import Data.Primitive.Array (sizeofArray)

import qualified Data.Eexpr.Text.Internal as Ffi

parse :: ByteString -> ([Error], Either [Error] [Eexpr Location])
parse input = runST $ do
  p <- Ffi.newDefaultParser input
  Ffi.parse p
  eexprs <- Ffi.drainEexprs p
  errs <- Ffi.drainErrors p
  warns <- Ffi.drainWarnings p
  Ffi.deinitParser p
  pure $ if sizeofArray errs == 0
    then (toList warns, Right $ toList eexprs)
    else (toList warns, Left $ toList errs)
