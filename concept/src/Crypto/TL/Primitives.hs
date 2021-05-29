module Crypto.TL.Primitives 
  ( Hash(..), Checksum, EncryptedHash
  , hashDefault, hashOnce
  , calcChecksum, verifyChecksum
  , randomHash
  , encrypt, decrypt
  , HashMode
  , hashFlipEndian
  , magicHash, hashToShortHash
  , hashToBS, bsToHash, incrementHash
  ) where

import qualified Crypto.Hash as Hash (hashWith, SHA256(..))
import qualified Crypto.Random.Types as CRT (getRandomBytes)
import qualified Data.ByteArray as ByteArray
import Data.ByteString (ByteString)
import Data.Bits (xor)
import Data.Serialize
import Data.Word (Word32)

import Crypto.TL.Types

hashOnce :: HashFunc -> Hash -> IO Hash
hashOnce hashFunc = hashFunc 1

hashDefault :: ByteString -> Hash
hashDefault = bsToHash . ByteArray.convert . Hash.hashWith Hash.SHA256

-- Add 1 to each byte (with overflow) and then hash to yield our checkpoint
calcChecksum :: HashFunc -> Hash -> IO Checksum
calcChecksum hashFunc = fmap Checksum . hashOnce hashFunc . incrementHash

verifyChecksum :: HashFunc -> Checksum -> Hash -> IO Bool
verifyChecksum hashFunc checksum = fmap (checksum ==) . calcChecksum hashFunc

-- To Encrypt, we xor the ending of tower n and the start of tower (n+1)
encrypt :: Hash -> Hash -> EncryptedHash
encrypt h1 h2 = EncryptedHash $ xorHash h1 h2

-- To Decrypt, we xor the ending of tower n and the result of the encrypt of tower (n+1)
decrypt :: Hash -> EncryptedHash -> Hash
decrypt hashKey (EncryptedHash eHash) = unEncryptedHash $ encrypt hashKey eHash

magicHash :: Hash
magicHash = hashFlipEndian $ Hash 
  0xad830d99 0x06e62640 0x93497b5e 0x51b56ff5
  0x6c798088 0x4a37aeb3 0x289352fa 0x702da3b9

hashToShortHash :: Hash -> ShortHash
hashToShortHash (Hash h7 h6 _ _ _ _ _ _) = ShortHash h7 h6

randomHash :: IO Hash
randomHash = (\(Right hash) -> hash) . runGet get <$> CRT.getRandomBytes 32

mapAccumRHash :: (acc -> Word32 -> (acc, Word32)) -> acc -> Hash -> (acc, Hash)
mapAccumRHash f acc (Hash h7 h6 h5 h4 h3 h2 h1 h0) = 
  (acc7, Hash h7' h6' h5' h4' h3' h2' h1' h0')
  where 
    (acc0, h0') = f acc h0
    (acc1, h1') = f acc0 h1
    (acc2, h2') = f acc1 h2
    (acc3, h3') = f acc2 h3
    (acc4, h4') = f acc3 h4
    (acc5, h5') = f acc4 h5
    (acc6, h6') = f acc5 h6
    (acc7, h7') = f acc6 h7

xorHash :: Hash -> Hash -> Hash
xorHash (Hash h7 h6 h5 h4 h3 h2 h1 h0) (Hash h7' h6' h5' h4' h3' h2' h1' h0') = 
  Hash 
    (h7 `xor` h7') (h6 `xor` h6') (h5 `xor` h5') (h4 `xor` h4') 
    (h3 `xor` h3') (h2 `xor` h2') (h1 `xor` h1') (h0 `xor` h0')

incrementHash :: Hash -> Hash
incrementHash = snd . mapAccumRHash f True
  where f False w = (False, w) 
        f True w = case w + 1 of 
                    0 -> (True, 0)
                    w' -> (False, w')

hashToBS :: Hash -> ByteString
hashToBS = runPut . put

bsToHash :: ByteString -> Hash
bsToHash = (\(Right hash) -> hash) . runGet get