{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Crypto.TL.Primitives 
  ( Hash, Checksum, EncryptedHash, Slow, Fast, Hashable(..)
  , hashDefault, hashOnce
  , calcChecksum, verifyChecksum
  , randomHash
  , encrypt, decrypt
  ) where

import Control.Monad (replicateM, when)

import qualified Crypto.Hash as Hash (hashWith, SHA256(..))

import qualified Crypto.Random.Types as CRT (getRandomBytes)

import Data.Bits (xor)

import qualified Data.ByteArray as ByteArray

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (copy, map, pack, packZipWith, unpack)
import Data.ByteString.Base16 (encodeBase16')

import Data.ByteString.Unsafe (unsafeUseAsCString)

import Data.Serialize

import Foreign.C.String (CString)

import System.IO.Unsafe (unsafePerformIO)

newtype Hash a = 
    Hash 
    { unHash :: ByteString
    } deriving Eq
  
instance Serialize (Hash a) where
  put (Hash bs) = 
    do
      let unpackedBS = BS.unpack bs
      when (length unpackedBS /= 32) $ error "Hash has bad length!"
      mapM_ putWord8 unpackedBS
  get = Hash . BS.pack <$> replicateM 32 getWord8

newtype Checksum a = 
    Checksum 
    { unChecksum :: Hash a
    } deriving (Eq, Serialize, Show)

newtype EncryptedHash a = 
    EncryptedHash 
    { unEncryptedHash :: Hash a
    } deriving (Eq, Serialize, Show)

instance Show (Hash a) where
    show = show . encodeBase16' . unHash

class Hashable a where
  hashIter :: Int -> Hash a -> Hash a

hashOnce :: Hashable a => Hash a -> Hash a
hashOnce = hashIter 1 

data Slow

instance Hashable Slow where
  hashIter num = iterate' num sha256'
    where
        iterate' :: Int -> (a -> a) -> a -> a
        iterate' n f ainit = iterate'' n ainit
            where 
                iterate'' i a
                    | i <= 0 = a
                    | otherwise = a' `seq` i' `seq` iterate' i' f a'
                        where
                        a' = f a 
                        i' = i-1

        sha256' :: Hash a -> Hash a
        sha256' = Hash . ByteArray.convert . Hash.hashWith Hash.SHA256 . unHash

data Fast

instance Hashable Fast where
  hashIter i hash = unsafePerformIO $ sha256iterFast'
    where
      sha256iterFast' :: IO (Hash Fast)
      sha256iterFast' =
          do
              let bs' = BS.copy $ unHash hash
              unsafeUseAsCString bs' (c_sha256_iter i)
              pure $ Hash bs'

hashDefault :: ByteString -> Hash a
hashDefault = Hash . ByteArray.convert . Hash.hashWith Hash.SHA256

randomHash :: IO (Hash a)
randomHash = Hash <$> CRT.getRandomBytes 32

-- Add 1 to each byte (with overflow) and then hash to yield our checkpoint
calcChecksum :: Hashable a => Hash a -> Checksum a
calcChecksum = Checksum . hashOnce . Hash . BS.map (+1) . unHash

verifyChecksum :: Hashable a => Checksum a -> Hash a -> Bool
verifyChecksum checksum hash = checksum == calcChecksum hash

-- To Encrypt, we xor the ending of tower n and the start of tower (n+1)
encrypt :: Hash a -> Hash a -> EncryptedHash a
encrypt (Hash bs1) (Hash bs2) = EncryptedHash $ Hash $ BS.packZipWith xor bs1 bs2

-- To Decrypt, we xor the ending of tower n and the result of the encrypt of tower (n+1)
decrypt :: Hash a -> EncryptedHash a -> Hash a
decrypt hashKey (EncryptedHash eHash) = unEncryptedHash $ encrypt hashKey eHash

-- fast simd c sha, optimized for iteration
foreign import ccall safe "sha256_iter"
  c_sha256_iter :: Int -> CString-> IO ()