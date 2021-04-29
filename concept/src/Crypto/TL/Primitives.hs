{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Crypto.TL.Primitives 
  ( Hash(..), Checksum, EncryptedHash, Hashable(..)
  , hashDefault, hashOnce
  , calcChecksum, verifyChecksum
  , randomHash
  , encrypt, decrypt
  , HashMode
  , unsafeUseAsCString
  ) where

import Control.Monad (replicateM, when)

import qualified Crypto.Hash as Hash (hashWith, SHA256(..))

import qualified Crypto.Random.Types as CRT (getRandomBytes)

import Data.Bits (shiftR, xor)

import qualified Data.ByteArray as ByteArray

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (mapAccumR, pack, packZipWith, unpack)
import Data.ByteString.Base16 (encodeBase16')
import qualified Data.ByteString.Unsafe as BS (unsafeUseAsCString)

import Data.Char (chr)
import Data.Proxy (Proxy(..))
import Data.Serialize
import Data.Word (Word16)

import Foreign.C.String (CString)

newtype Hash = 
  Hash 
  { unHash :: ByteString
  } deriving Eq

instance Show Hash where
  show = map (chr . fromEnum) . BS.unpack . encodeBase16' . unHash
  
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
    } deriving (Eq, Serialize)

instance Show Checksum where
  show = show . unChecksum

newtype EncryptedHash = 
  EncryptedHash 
  { unEncryptedHash :: Hash
  } deriving (Eq, Serialize)

instance Show EncryptedHash where
  show = show . unEncryptedHash

class Hashable a where
  hashIter :: Proxy a -> Int -> Hash -> Hash

type HashMode a = Proxy a

hashOnce :: Hashable a => HashMode a -> Hash -> Hash
hashOnce mode = hashIter mode 1

hashDefault :: ByteString -> Hash
hashDefault = Hash . ByteArray.convert . Hash.hashWith Hash.SHA256

randomHash :: IO Hash
randomHash = Hash <$> CRT.getRandomBytes 32

-- Add 1 to each byte (with overflow) and then hash to yield our checkpoint
calcChecksum :: Hashable a => HashMode a -> Hash -> Checksum
calcChecksum mode = Checksum . hashOnce mode . Hash . incrementBS . unHash

  -- uses Word16 to store overflow in 9th bit and shifts 8 bits to add to next byte
incrementBS :: ByteString -> ByteString
incrementBS = snd . BS.mapAccumR f (1 :: Word16)
  where f acc w = (shiftR acc' 8, fromIntegral acc')
          where acc' = acc + fromIntegral w

verifyChecksum :: Hashable a => HashMode a -> Checksum -> Hash -> Bool
verifyChecksum mode checksum hash = checksum == calcChecksum mode hash

-- To Encrypt, we xor the ending of tower n and the start of tower (n+1)
encrypt :: Hash -> Hash -> EncryptedHash
encrypt (Hash bs1) (Hash bs2) = EncryptedHash $ Hash $ BS.packZipWith xor bs1 bs2

-- To Decrypt, we xor the ending of tower n and the result of the encrypt of tower (n+1)
decrypt :: Hash -> EncryptedHash -> Hash
decrypt hashKey (EncryptedHash eHash) = unEncryptedHash $ encrypt hashKey eHash

unsafeUseAsCString :: Hash -> (CString -> IO a) -> IO a
unsafeUseAsCString = BS.unsafeUseAsCString . unHash

