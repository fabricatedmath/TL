{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Crypto.TL.Primitives.EncryptedHash 
  ( EncryptedHash, encryptHash, decryptHash
  ) where

import Data.Bits (xor)
import Data.Serialize

import Crypto.TL.Primitives.Hash

newtype EncryptedHash = 
  EncryptedHash 
  { unEncryptedHash :: Hash
  } deriving (Eq, Serialize)

instance Show EncryptedHash where
  show = show . unEncryptedHash

xorHash :: Hash -> Hash -> Hash
xorHash (Hash h7 h6 h5 h4 h3 h2 h1 h0) (Hash h7' h6' h5' h4' h3' h2' h1' h0') = 
  Hash 
    (h7 `xor` h7') (h6 `xor` h6') (h5 `xor` h5') (h4 `xor` h4') 
    (h3 `xor` h3') (h2 `xor` h2') (h1 `xor` h1') (h0 `xor` h0')

-- To Encrypt, we xor the ending of tower n and the start of tower (n+1)
encryptHash :: Hash -> Hash -> EncryptedHash
encryptHash h1 h2 = EncryptedHash $ xorHash h1 h2

-- To Decrypt, we xor the ending of tower n and the result of the encrypt of tower (n+1)
decryptHash :: Hash -> EncryptedHash -> Hash
decryptHash hashKey (EncryptedHash eHash) = unEncryptedHash $ encryptHash hashKey eHash