module Crypto.TL.Impls.Util where

import qualified Data.ByteString as BS (copy)
import qualified Data.ByteString.Unsafe as BS (unsafeUseAsCString)
import Data.Int (Int32)
import Foreign.C.String (CString)
import System.IO.Unsafe (unsafePerformIO)

import Crypto.TL.Primitives
import Crypto.TL.Types

availabilityHelper :: Enum a => IO Int32 -> IO a
availabilityHelper = fmap $ toEnum . fromEnum
{-# INLINABLE availabilityHelper #-}

iterateHashHelper :: (Int -> CString -> IO ()) -> HashFunc
iterateHashHelper ffiHashFunction numHashes hash = unsafePerformIO $ iterateHash'
  where 
    iterateHash' :: IO Hash
    iterateHash' = do
      let bs' = BS.copy $ unHash hash
      BS.unsafeUseAsCString bs' (ffiHashFunction numHashes)
      pure $ Hash bs'
{-# INLINABLE iterateHashHelper #-}