{-# LANGUAGE TemplateHaskell #-}

module Crypto.TL.Hashing.Impls.Cuda where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Unsafe as BS (unsafeUseAsCString)
import Data.FileEmbed (embedFileIfExists)
import Data.Int (Int32)
import Data.Proxy (Proxy(..))

import Foreign.C.String (CString)
import Foreign.ForeignPtr
import Foreign.Ptr

import Crypto.TL.Hashing.Util
import Crypto.TL.Types

data ShaCuda

shaModeCuda :: HashMode ShaCuda
shaModeCuda = Proxy

--  enum Availability { Available, NotCompiled, NoNvidiaDriver };
data Availability = Available | NotCompiled | NoNvidiaDriver
  deriving (Enum, Eq, Show)

data ShaCudaHandle

cudaIsAvailable :: IO Availability
cudaIsAvailable = toEnum . fromEnum <$> c_cudaIsAvailable

maybeCudaFatBin :: Maybe ByteString
maybeCudaFatBin = $(embedFileIfExists "lib/build/sha-impls/cuda-sha/sha256_iter.fatbin")

cudaInit :: ForeignPtr ShaCudaHandle -> IO Int32
cudaInit shaCuda =  
  case maybeCudaFatBin of
    Nothing -> error "Trying to init cuda, but no cuda fatbin found!"
    Just cudaFatBin -> 
      withForeignPtr shaCuda (\ptr -> 
        BS.unsafeUseAsCString cudaFatBin $ c_cudaInit ptr
      )

-- TODO: Remove errcode printing
cudaCreateChains :: ForeignPtr ShaCudaHandle -> Int -> [Hash] -> IO [Tower]
cudaCreateChains shaCuda numIters startingHashes = do  
  let hashbs = toPacked startingHashes
      numHashes = length startingHashes
  endingHashes <- withForeignPtr shaCuda (\ptr -> do
      errcode <- BS.unsafeUseAsCString hashbs $ c_cudaCreateChains ptr (fromIntegral numHashes) (fromIntegral numIters)
      print errcode
      return $ toUnpacked hashbs
    )
  return $ zipWith (Tower numIters) startingHashes endingHashes

instance HasBulkHashFunc ShaCuda where
  getBulkHashFunc _ = do
    a <- cudaIsAvailable
    case a of
      Available -> do
        shaCuda <- newCudaSha
        errCode <- cudaInit shaCuda
        case errCode of
          0 -> return $ return $ cudaCreateChains shaCuda
          _ -> return $ Left "Failed to Init Cuda"
      NotCompiled -> return $ Left "Not Compiled"
      NoNvidiaDriver -> return $ Left "No NVida Driver"
    where
      newCudaSha :: IO (ForeignPtr ShaCudaHandle)
      newCudaSha = c_cudaNew >>= newForeignPtr c_cudaDelete
  
foreign import ccall safe "cudaIsAvailable"
  c_cudaIsAvailable :: IO Int32

foreign import ccall safe "cudaNew" c_cudaNew
    :: IO (Ptr ShaCudaHandle)

foreign import ccall safe "cudaInit" c_cudaInit
    :: Ptr ShaCudaHandle -> CString -> IO Int32

-- change types to 64 bit
foreign import ccall safe "cudaCreateChains" c_cudaCreateChains
    :: Ptr ShaCudaHandle -> Int32 -> Int32 -> CString -> IO Int32

foreign import ccall safe "&cudaDelete" c_cudaDelete
    :: FinalizerPtr ShaCudaHandle