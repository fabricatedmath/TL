module Crypto.TL.Hashing.Impls.Generic 
  ( shaModeGeneric, shaModeBulkGeneric
  ) where

import Data.Int (Int32)
import Data.Proxy (Proxy(..))
import Foreign.C.String

import Crypto.TL.Hashing.Util
import Crypto.TL.Types

data ShaGeneric

shaModeGeneric :: HashMode ShaGeneric
shaModeGeneric = Proxy

shaModeBulkGeneric :: HashMode (Bulk ShaGeneric)
shaModeBulkGeneric = Proxy

instance HasHashFunc ShaGeneric where
  getHashFunc _ = do
    a <- availabilityHelper c_isAvailable
    return $ case a of
      Available -> Right $ iterateHashHelper c_iterateHash

--  enum Availability { Available };
data Availability = Available
  deriving (Enum, Eq, Show)

foreign import ccall safe "isAvailable_generic"
  c_isAvailable :: IO Int32

foreign import ccall safe "iterateHash_generic"
  c_iterateHash :: Int -> CString -> IO ()