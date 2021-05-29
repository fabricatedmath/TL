module Crypto.TL.Primitives.ShortHash 
  ( ShortHash(..)
  , hashToShortHash, shortHashFlipEndian
  ) where

import Data.Serialize
import Data.Word (Word32, byteSwap32)
import Text.Printf (printf)

import Crypto.TL.Primitives.Hash

data ShortHash = ShortHash {-# UNPACK #-} !Word32 !Word32
  deriving Eq

instance Show ShortHash where
  show shortHash = printf formatString h7 h6
    where (ShortHash h7 h6) = shortHashFlipEndian shortHash
          formatString = concat $ replicate 2 "%08x"

instance Serialize ShortHash where
  put (ShortHash h7 h6) = do
    putWord32be h7
    putWord32be h6
  get = 
    ShortHash <$> getWord32be <*> getWord32be

hashToShortHash :: Hash -> ShortHash
hashToShortHash (Hash h7 h6 _ _ _ _ _ _) = ShortHash h7 h6

shortHashFlipEndian :: ShortHash -> ShortHash
shortHashFlipEndian (ShortHash h7 h6) = ShortHash (byteSwap32 h7) (byteSwap32 h6)