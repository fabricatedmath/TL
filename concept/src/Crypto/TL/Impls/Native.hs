module Crypto.TL.Impls.Native 
  ( shaModeNative
  ) where

import qualified Crypto.Hash as Hash (hashWith, SHA256(..))

import qualified Data.ByteArray as ByteArray

import Data.Proxy (Proxy(..))

import Crypto.TL.Primitives
import Crypto.TL.Types

data Native

shaModeNative :: HashMode Native
shaModeNative = Proxy

instance HasHashFunc Native where
  getHashFunc _ = return $ Right $ hashIter
    where
      hashIter :: Int -> Hash -> IO Hash
      hashIter num = return . hashFlipEndian . iterate' num sha256' . hashFlipEndian
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