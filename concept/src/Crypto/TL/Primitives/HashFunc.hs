module Crypto.TL.Primitives.HashFunc 
  ( HashFunc, HashMode, HasHashFunc(..)
  , hashByteString, hashOnce
  ) where

import qualified Crypto.Hash as Hash (hashWith, SHA256(..))
import qualified Data.ByteArray as ByteArray
import Data.ByteString (ByteString)
import Data.Proxy (Proxy)

import Crypto.TL.Primitives.Hash

type HashMode a = Proxy a

type HashFunc = Int -> Hash -> IO Hash

class HasHashFunc a where
  getHashFunc :: HashMode a -> IO (Either String HashFunc)

hashOnce :: HashFunc -> Hash -> IO Hash
hashOnce hashFunc = hashFunc 1

hashByteString :: ByteString -> Hash
hashByteString = bsToHash . ByteArray.convert . Hash.hashWith Hash.SHA256