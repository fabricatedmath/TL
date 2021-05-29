module Crypto.TL.Hashing.Impls.X86 
  ( shaModeX86, shaModeBulkX86
  ) where

import Data.Int (Int32)
import Data.Proxy (Proxy(..))
import Foreign.C.String

import Crypto.TL.Hashing.Util
import Crypto.TL.Primitives.BulkHashFunc
import Crypto.TL.Primitives.HashFunc

data ShaX86

shaModeX86 :: HashMode ShaX86
shaModeX86 = Proxy

shaModeBulkX86 :: HashMode (Bulk ShaX86)
shaModeBulkX86 = Proxy

instance HasHashFunc ShaX86 where
  getHashFunc _ = do
    a <- availabilityHelper c_isAvailable
    return $ case a of
      Available -> Right $ iterateHashHelper c_iterateHash
      NotCompiled -> Left "Not compiled"
      NoSSE41 -> Left "No CPU x86 SSE4.1 extension capability found"
      NoSha -> Left "No CPU x86 SHA extension capability found"

--  enum Availability { Available, NotCompiled, NoSSE41, NoSha };
data Availability = Available | NotCompiled | NoSSE41 | NoSha
  deriving (Enum, Eq, Show)

foreign import ccall safe "isAvailable_x86"
  c_isAvailable :: IO Int32

foreign import ccall safe "iterateHash_x86"
  c_iterateHash :: Int -> CString -> IO ()