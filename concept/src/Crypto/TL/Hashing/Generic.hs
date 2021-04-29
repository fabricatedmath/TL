module Crypto.TL.Hashing.Generic 
  ( shaGenericMode
  ) where

import Crypto.TL.Primitives

import qualified Data.ByteString as BS (copy)
import qualified Data.ByteString.Unsafe as BS (unsafeUseAsCString)

import Data.Proxy (Proxy(..))

import Foreign.C.String (CString)

import System.IO.Unsafe (unsafePerformIO)

data SHAGeneric

type SHAGenericMode = HashMode SHAGeneric

shaGenericMode :: HashMode SHAGeneric
shaGenericMode = Proxy

instance Hashable SHAGeneric where
  hashIter _ i hash = unsafePerformIO $ sha256iterFast'
    where
      sha256iterFast' :: IO Hash
      sha256iterFast' =
        do
          let bs' = BS.copy $ unHash hash
          BS.unsafeUseAsCString bs' (c_sha256_iter i)
          pure $ Hash bs'

foreign import ccall safe "sha256_iter"
  c_sha256_iter :: Int -> CString-> IO ()
