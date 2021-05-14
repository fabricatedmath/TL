{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Crypto.TL.Primitives 
  ( Hash(..), Checksum, EncryptedHash
  , hashDefault, hashOnce
  , calcChecksum, verifyChecksum
  , randomHash
  , encrypt, decrypt
  , HashMode
  , unsafeUseAsCString
  , flipEndian
  ) where

import qualified Crypto.Hash as Hash (hashWith, SHA256(..))
import Control.Monad (replicateM)
import qualified Crypto.Random.Types as CRT (getRandomBytes)
import Data.Bits (shiftR, xor)
import qualified Data.ByteArray as ByteArray
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (mapAccumR)
import qualified Data.ByteString.Unsafe as BS (unsafeUseAsCString)
import Data.Serialize
import Data.Word (Word16,Word32)
import Foreign.C.String (CString)

import Crypto.TL.Types

hashOnce :: HashFunc -> Hash -> Hash
hashOnce hashFunc = hashFunc 1

hashDefault :: ByteString -> Hash
hashDefault = Hash . ByteArray.convert . Hash.hashWith Hash.SHA256

randomHash :: IO Hash
randomHash = Hash <$> CRT.getRandomBytes 32

-- Add 1 to each byte (with overflow) and then hash to yield our checkpoint
calcChecksum :: HashFunc -> Hash -> Checksum
calcChecksum hashFunc = Checksum . hashOnce hashFunc . Hash . incrementBS . unHash

  -- uses Word16 to store overflow in 9th bit and shifts 8 bits to add to next byte
incrementBS :: ByteString -> ByteString
incrementBS = snd . BS.mapAccumR f (1 :: Word16)
  where f acc w = (shiftR acc' 8, fromIntegral acc')
          where acc' = acc + fromIntegral w

verifyChecksum :: HashFunc -> Checksum -> Hash -> Bool
verifyChecksum hashFunc checksum hash = checksum == calcChecksum hashFunc hash

-- To Encrypt, we xor the ending of tower n and the start of tower (n+1)
-- TODO: can replace with packZipWith in bytestring-0.11
encrypt :: Hash -> Hash -> EncryptedHash
encrypt (Hash bs1) (Hash bs2) = EncryptedHash $ Hash $ fromWord64 $ zipWith xor (toWord64 bs1) (toWord64 bs2)
  where
    toWord64 = fromRight . runGet (replicateM 4 getWord64host)
    fromWord64 = runPut . mapM_ putWord64host

-- To Decrypt, we xor the ending of tower n and the result of the encrypt of tower (n+1)
decrypt :: Hash -> EncryptedHash -> Hash
decrypt hashKey (EncryptedHash eHash) = unEncryptedHash $ encrypt hashKey eHash

unsafeUseAsCString :: Hash -> (CString -> IO a) -> IO a
unsafeUseAsCString = BS.unsafeUseAsCString . unHash

flipEndian :: Hash -> Hash
flipEndian (Hash bs) = 
  let
    getLE :: Get [Word32]
    getLE = replicateM 8 getWord32le

    putBE :: Putter [Word32]
    putBE = mapM_ putWord32be

    Right leWords = runGet getLE bs
  in
    Hash $ runPut (putBE leWords)



