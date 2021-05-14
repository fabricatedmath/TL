module Crypto.TL.Impls.X86 
  ( shaModeX86
  ) where

import Data.Int (Int32)
import Data.Proxy (Proxy(..))
import Foreign.C.String

import Crypto.TL.Impls.Util
import Crypto.TL.Types

data ShaX86

shaModeX86 :: HashMode ShaX86
shaModeX86 = Proxy

instance HasHashFunc ShaX86 where
  getHashFunc _ = do
    a <- availabilityHelper c_isAvailable
    return $ case a == Available of
      True -> Right $ iterateHashHelper c_iterateHash
      False -> Left $ availabilityMessage a
    where 
      availabilityMessage :: Availability -> String
      availabilityMessage availability = 
        case availability of
          Available -> "Available"
          NotCompiled -> "Not compiled"
          NoSSE41 -> "No CPU x86 SSE4.1 extension capability found"
          NoSha -> "No CPU x86 SHA extension capability found"

--  enum Availability { Available, NotCompiled, NoSSE41, NoSha };
data Availability = Available | NotCompiled | NoSSE41 | NoSha
  deriving (Enum, Eq, Show)

foreign import ccall safe "isAvailable_x86"
  c_isAvailable :: IO Int32

foreign import ccall safe "iterateHash_x86"
  c_iterateHash :: Int -> CString -> IO ()