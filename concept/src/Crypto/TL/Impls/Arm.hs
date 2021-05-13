module Crypto.TL.Impls.Arm where

import Data.Int (Int32)

import Data.Proxy (Proxy(..))

import Foreign.C.String

import Crypto.TL.Util

data ShaArm

shaModeArm :: HashMode ShaArm
shaModeArm = Proxy

instance FFIHashable ShaArm where
  ffiHashFunc _ = do
    a <- availabilityHelper c_isAvailable_arm
    case a == Available of
      True -> return $ Right $ iterateHashHelper c_iterateHash_arm
      False -> return $ Left $ availabilityMessage a
    where 
      availabilityMessage :: Availability -> String
      availabilityMessage availability = 
        case availability of
          Available -> "Available"
          NotCompiled -> "Not Compiled"
          NoSha -> "No Arm Sha extension capability found on CPU"

--  enum Availability { Available, NotCompiled, NoSha };
data Availability = Available | NotCompiled | NoSha
  deriving (Enum, Eq, Show)

foreign import ccall safe "isAvailable_arm"
  c_isAvailable_arm :: IO Int32

foreign import ccall safe "iterateHash_arm"
  c_iterateHash_arm :: Int -> CString -> IO ()