{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (readFile)
import qualified Data.ByteString.Unsafe as BS (unsafeUseAsCString)
import Data.FileEmbed (embedFileIfExists)
import Data.Int (Int32)

import Foreign.C.String (CString)
import Foreign.ForeignPtr
import Foreign.Ptr

data CudaAvailability = Available | NotCompiled | NoNvidiaDriver
  deriving (Enum, Show)

main :: IO ()
main = do
  print "dogs"
  c_x86IsAvailable >>= print
  c_armIsAvailable >>= print
  c_cudaIsAvailable >>= print

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

cudaIsAvailable :: IO CudaAvailability
cudaIsAvailable = toEnum . fromIntegral <$> c_cudaIsAvailable

newCudaSha :: IO CudaSha
newCudaSha = fmap CudaSha $ c_cudaNew >>= newForeignPtr c_cudaDelete

maybeCudaFatBin :: Maybe ByteString
maybeCudaFatBin = $(embedFileIfExists "lib/build/sha-impls/cuda-sha/sha256_iter.fatbin")

cudaInit :: CudaSha -> IO Int32
cudaInit cudaSha =  
  case maybeCudaFatBin of
    Nothing -> error "Trying to init cuda, but no cuda fatbin found!"
    Just cudaFatBin -> 
      withForeignPtr (_cudaShaHandle cudaSha) (\ptr -> 
        BS.unsafeUseAsCString cudaFatBin $ c_cudaInit ptr 
      )

foreign import ccall safe "x86IsAvailable"
  c_x86IsAvailable :: IO Bool

foreign import ccall safe "armIsAvailable"
  c_armIsAvailable :: IO Bool

foreign import ccall safe "cudaIsAvailable"
  c_cudaIsAvailable :: IO Int32

foreign import ccall unsafe "cudaNew" c_cudaNew
    :: IO (Ptr CudaShaHandle)

foreign import ccall unsafe "cudaInit" c_cudaInit
    :: Ptr CudaShaHandle -> CString -> IO Int32

foreign import ccall unsafe "&cudaDelete" c_cudaDelete
    :: FinalizerPtr CudaShaHandle
