module Crypto.TL.Hashing.Util where

import qualified Data.ByteString.Unsafe as BS (unsafeUseAsCString)
import Data.Int (Int32)
import Foreign.C.String (CString)

import Crypto.TL.Primitives.Hash
import Crypto.TL.Primitives.HashFunc

availabilityHelper :: Enum a => IO Int32 -> IO a
availabilityHelper = fmap $ toEnum . fromEnum
{-# INLINABLE availabilityHelper #-}

iterateHashHelper :: (Int -> CString -> IO ()) -> HashFunc
iterateHashHelper ffiHashFunction numHashes hash = do
  let bs' = hashToBS hash
  BS.unsafeUseAsCString bs' (ffiHashFunction numHashes)
  pure $ bsToHash bs'
{-# INLINABLE iterateHashHelper #-}