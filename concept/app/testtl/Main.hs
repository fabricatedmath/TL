{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Unsafe as BS (unsafeUseAsCString)
import Data.FileEmbed
import Data.Int (Int32)

import Foreign.C.String (CString)
import Foreign.ForeignPtr
import Foreign.Ptr



main :: IO ()
main = do
  print "dogs"
  x86IsAvailable >>= print
  armIsAvailable >>= print
  cudaIsAvailable >>= print
  cuda <- newCudaSha
  i <- cudaInit cuda
  print i
  print "cats"

  --print cudaFatBin


data CudaShaHandle

data CudaSha = 
  CudaSha 
  { _cudaShaHandle :: ForeignPtr CudaShaHandle
  }

newCudaSha :: IO CudaSha
newCudaSha = fmap CudaSha $ c_cudaNew >>= newForeignPtr c_cudaDelete

-- embed cuda fatbin file into this function (no relative paths for fatbin file)
cudaFatBin :: ByteString
cudaFatBin = $(embedFile "tl-lib/build/sha256-impls/sha-cuda/sha256_iter.fatbin")

cudaInit :: CudaSha -> IO Int32
cudaInit cudaSha =  
  withForeignPtr (_cudaShaHandle cudaSha) (\ptr -> 
    BS.unsafeUseAsCString cudaFatBin $ c_cudaInit ptr 
  )

foreign import ccall safe "x86IsAvailable"
  x86IsAvailable :: IO Bool

foreign import ccall safe "armIsAvailable"
  armIsAvailable :: IO Bool

foreign import ccall safe "cudaIsAvailable"
  cudaIsAvailable :: IO Int

foreign import ccall unsafe "cudaNew" c_cudaNew
    :: IO (Ptr CudaShaHandle)

foreign import ccall unsafe "cudaInit" c_cudaInit
    :: Ptr CudaShaHandle -> CString -> IO Int32

foreign import ccall unsafe "&cudaDelete" c_cudaDelete
    :: FinalizerPtr CudaShaHandle
