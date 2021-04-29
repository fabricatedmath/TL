module Crypto.TL.Hashing.X86 
  ( shax86Mode 
  ) where

import Crypto.TL.Primitives

import qualified Data.ByteString as BS (copy)
import qualified Data.ByteString.Unsafe as BS (unsafeUseAsCString)

import Data.Proxy (Proxy(..))

import Foreign.C.String (CString)

import System.IO.Unsafe (unsafePerformIO)

data SHAX86

shax86Mode :: HashMode SHAX86
shax86Mode = Proxy

instance Hashable SHAX86 where
  hashIter _ i hash = unsafePerformIO $ sha256iterFast'
    where
      sha256iterFast' :: IO Hash
      sha256iterFast' =
        do
          let bs' = BS.copy $ unHash hash
          BS.unsafeUseAsCString bs' (c_sha256_iter i)
          pure $ Hash bs'

-- fast simd c sha, optimized for iteration
foreign import ccall safe "sha256_x86_iter"
  c_sha256_iter :: Int -> CString-> IO ()
