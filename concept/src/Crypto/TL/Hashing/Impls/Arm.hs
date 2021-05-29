module Crypto.TL.Hashing.Impls.Arm 
  ( shaModeArm, shaModeBulkArm
  ) where

import Data.Int (Int32)
import Data.Proxy (Proxy(..))
import Foreign.C.String

import Crypto.TL.Hashing.Util
import Crypto.TL.Primitives.BulkHashFunc
import Crypto.TL.Primitives.HashFunc

data ShaArm

shaModeArm :: HashMode ShaArm
shaModeArm = Proxy

shaModeBulkArm :: HashMode (Bulk ShaArm)
shaModeBulkArm = Proxy

instance HasHashFunc ShaArm where
  getHashFunc _ = do
    a <- availabilityHelper c_isAvailable
    return $ case a of
      Available -> Right $ iterateHashHelper c_iterateHash
      NotCompiled -> Left "Not Compiled"
      NoSha -> Left "No CPU Arm Sha extension capability found"

--  enum Availability { Available, NotCompiled, NoSha };
data Availability = Available | NotCompiled | NoSha
  deriving (Enum, Eq, Show)

foreign import ccall safe "isAvailable_arm"
  c_isAvailable :: IO Int32

foreign import ccall safe "iterateHash_arm"
  c_iterateHash :: Int -> CString -> IO ()