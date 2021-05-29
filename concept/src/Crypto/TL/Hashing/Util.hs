module Crypto.TL.Hashing.Util where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (length)
import qualified Data.ByteString.Unsafe as BS (unsafeUseAsCString)
import Data.Int (Int32)
import Foreign.C.String (CString)

import Control.Monad (replicateM)

import Data.Serialize (runGet, get, runPut, put)

import Crypto.TL.Primitives
import Crypto.TL.Types

availabilityHelper :: Enum a => IO Int32 -> IO a
availabilityHelper = fmap $ toEnum . fromEnum
{-# INLINABLE availabilityHelper #-}

iterateHashHelper :: (Int -> CString -> IO ()) -> HashFunc
iterateHashHelper ffiHashFunction numHashes hash = do
  let bs' = hashToBS hash
  BS.unsafeUseAsCString bs' (ffiHashFunction numHashes)
  pure $ bsToHash bs'
{-# INLINABLE iterateHashHelper #-}

toPacked :: [Hash] -> ByteString
toPacked = runPut . mapM_ put

toUnpacked :: ByteString -> [Hash]
toUnpacked bs = hashes
  where
    Right hashes = runGet (replicateM numHashes get) bs
    numHashes = BS.length bs `div` 32