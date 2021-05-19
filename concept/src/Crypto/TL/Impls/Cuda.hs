{-# LANGUAGE TemplateHaskell #-}

module Crypto.TL.Impls.Cuda where

--import Control.Monad (void)

import Data.ByteString (ByteString)
--import qualified Data.ByteString as BS (readFile)
import qualified Data.ByteString.Unsafe as BS (unsafeUseAsCString)
import Data.FileEmbed (embedFileIfExists)
import Data.Int (Int32)
import Data.Proxy (Proxy(..))

import Foreign.C.String (CString)
import Foreign.ForeignPtr
import Foreign.Ptr

--import System.IO.Unsafe (unsafePerformIO)

import Crypto.TL.Bulk
import Crypto.TL.Types

data ShaCuda

shaModeCuda :: HashMode ShaCuda
shaModeCuda = Proxy

--  enum Availability { Available, NotCompiled, NoNvidiaDriver };
data Availability = Available | NotCompiled | NoNvidiaDriver
  deriving (Enum, Eq, Show)

data CudaShaHandle

data CudaSha = 
  CudaSha 
  { _cudaShaHandle :: ForeignPtr CudaShaHandle
  }

cudaIsAvailable :: IO Availability
cudaIsAvailable = toEnum . fromEnum <$> c_cudaIsAvailable

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

--cudaCreateChains :: CudaSha -> Int -> [Hash] -> [Hash]
--cudaCreateChains cudaSha numIters hashes = unsafePerformIO $ cudaCreateChains' cudaSha numIters hashes

cudaCreateChains :: CudaSha -> Int -> [Hash] -> IO [Hash]
cudaCreateChains cudaSha numIters hashes = do  
  let hashbs = toPacked hashes
      numHashes = length hashes
  withForeignPtr (_cudaShaHandle cudaSha) (\ptr -> do
      errcode <- BS.unsafeUseAsCString hashbs $ c_cudaCreateChains ptr (fromIntegral numHashes) (fromIntegral numIters)
      print errcode
      return $ toUnpacked hashbs
    )
  


foreign import ccall safe "cudaIsAvailable"
  c_cudaIsAvailable :: IO Int32

foreign import ccall safe "cudaNew" c_cudaNew
    :: IO (Ptr CudaShaHandle)

foreign import ccall safe "cudaInit" c_cudaInit
    :: Ptr CudaShaHandle -> CString -> IO Int32

-- change types to 64 bit
foreign import ccall safe "cudaCreateChains" c_cudaCreateChains
    :: Ptr CudaShaHandle -> Int32 -> Int32 -> CString -> IO Int32

foreign import ccall safe "&cudaDelete" c_cudaDelete
    :: FinalizerPtr CudaShaHandle

instance HasBulkHashFunc ShaCuda where
  getBulkHashFunc _ = do
    a <- cudaIsAvailable
    case a of
      Available -> do
        cudaSha <- newCudaSha
        errCode <- cudaInit cudaSha
        case errCode of
          0 -> return $ return $ cudaCreateChains cudaSha
          _ -> return $ Left "Failed to init cuda"
      NotCompiled -> return $ Left "Not Compiled"
      NoNvidiaDriver -> return $ Left "No NVida Driver"
