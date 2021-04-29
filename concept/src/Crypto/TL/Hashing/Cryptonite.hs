module Crypto.TL.Hashing.Cryptonite 
  ( cryptoniteMode
  ) where

import qualified Crypto.Hash as Hash (hashWith, SHA256(..))

import qualified Data.ByteArray as ByteArray

import Data.Proxy (Proxy(..))

import Crypto.TL.Primitives

data Cryptonite

type CryptoniteMode = HashMode Cryptonite

cryptoniteMode :: HashMode Cryptonite
cryptoniteMode = Proxy

instance Hashable Cryptonite where
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