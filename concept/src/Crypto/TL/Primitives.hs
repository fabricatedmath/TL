{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Crypto.TL.Primitives 
  ( Hash, Checksum, EncryptedHash, Slow, Fast, Hashable(..)
  , hashDefault, hashOnce
  , calcChecksum, verifyChecksum
  , randomHash
  , encrypt, decrypt
  , slowMode, fastMode, HashMode
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

import Data.Proxy (Proxy(..))

import Data.Serialize

import Foreign.C.String (CString)

import System.IO.Unsafe (unsafePerformIO)

newtype Hash = 
    Hash 
    { unHash :: ByteString
    } deriving Eq
  
instance Serialize Hash where
  put (Hash bs) = 
    do
      let unpackedBS = BS.unpack bs
      when (length unpackedBS /= 32) $ error "Hash has bad length!"
      mapM_ putWord8 unpackedBS
  get = Hash . BS.pack <$> replicateM 32 getWord8

newtype Checksum = 
    Checksum 
    { unChecksum :: Hash
    } deriving (Eq, Serialize, Show)

newtype EncryptedHash = 
    EncryptedHash 
    { unEncryptedHash :: Hash
    } deriving (Eq, Serialize, Show)

instance Show Hash where
    show = show . encodeBase16' . unHash

class Hashable a where
  hashIter :: Proxy a -> Int -> Hash -> Hash

type HashMode a = Proxy a

hashOnce :: Hashable a => HashMode a -> Hash -> Hash
hashOnce mode = hashIter mode 1

data Slow

slowMode :: HashMode Slow
slowMode = Proxy

instance Hashable Slow where
  hashIter _ num = iterate' num sha256'
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

        sha256' :: Hash -> Hash
        sha256' = Hash . ByteArray.convert . Hash.hashWith Hash.SHA256 . unHash

data Fast

fastMode :: HashMode Fast
fastMode = Proxy

instance Hashable Fast where
  hashIter _ i hash = unsafePerformIO $ sha256iterFast'
    where
      sha256iterFast' :: IO Hash
      sha256iterFast' =
          do
              let bs' = BS.copy $ unHash hash
              unsafeUseAsCString bs' (c_sha256_iter i)
              pure $ Hash bs'

hashDefault :: ByteString -> Hash
hashDefault = Hash . ByteArray.convert . Hash.hashWith Hash.SHA256

randomHash :: IO Hash
randomHash = Hash <$> CRT.getRandomBytes 32

-- Add 1 to each byte (with overflow) and then hash to yield our checkpoint
calcChecksum :: Hashable a => HashMode a -> Hash -> Checksum
calcChecksum mode = Checksum . hashOnce mode . Hash . BS.map (+1) . unHash

verifyChecksum :: Hashable a => HashMode a -> Checksum -> Hash -> Bool
verifyChecksum mode checksum hash = checksum == calcChecksum mode hash

-- To Encrypt, we xor the ending of tower n and the start of tower (n+1)
encrypt :: Hash -> Hash -> EncryptedHash
encrypt (Hash bs1) (Hash bs2) = EncryptedHash $ Hash $ BS.packZipWith xor bs1 bs2

-- To Decrypt, we xor the ending of tower n and the result of the encrypt of tower (n+1)
decrypt :: Hash -> EncryptedHash -> Hash
decrypt hashKey (EncryptedHash eHash) = unEncryptedHash $ encrypt hashKey eHash

-- fast simd c sha, optimized for iteration
foreign import ccall safe "sha256_iter"
  c_sha256_iter :: Int -> CString-> IO ()