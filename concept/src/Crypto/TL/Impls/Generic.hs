module Crypto.TL.Impls.Generic 
  ( shaModeGeneric
  ) where

import Data.Int (Int32)

import Data.Proxy (Proxy(..))

import Foreign.C.String

import Crypto.TL.Util

data ShaGeneric

shaModeGeneric :: HashMode ShaGeneric
shaModeGeneric = Proxy

instance FFIHashable ShaGeneric where
  ffiHashFunc _ = do
    a <- availabilityHelper c_isAvailable
    return $ case a == Available of
      True -> Right $ iterateHashHelper c_iterateHash
      False -> Left $ availabilityMessage a
    where 
      availabilityMessage :: Availability -> String
      availabilityMessage availability = 
        case availability of
          Available -> "Available"

--  enum Availability { Available };
data Availability = Available
  deriving (Enum, Eq, Show)

foreign import ccall safe "isAvailable_generic"
  c_isAvailable :: IO Int32

foreign import ccall safe "iterateHash_generic"
  c_iterateHash :: Int -> CString -> IO ()