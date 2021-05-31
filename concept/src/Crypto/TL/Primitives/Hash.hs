module Crypto.TL.Primitives.Hash 
  ( Hash(..)
  , hashFlipEndian, randomHash, bsToHash, hashToBS
  ) where

import qualified Crypto.Random.Types as CRT (getRandomBytes)

import Data.ByteString (ByteString)
import Data.Serialize
import Data.Word (Word32, byteSwap32)
import Text.Printf (printf)

data Hash = 
  Hash 
  {-# UNPACK #-} !Word32
  {-# UNPACK #-} !Word32
  {-# UNPACK #-} !Word32
  {-# UNPACK #-} !Word32
  {-# UNPACK #-} !Word32
  {-# UNPACK #-} !Word32
  {-# UNPACK #-} !Word32
  {-# UNPACK #-} !Word32
  deriving Eq

instance Show Hash where
  show hash = printf formatString h7 h6 h5 h4 h3 h2 h1 h0
    where (Hash h7 h6 h5 h4 h3 h2 h1 h0) = hashFlipEndian hash
          formatString = concat $ replicate 8 "%08x"
  
instance Serialize Hash where
  put (Hash h7 h6 h5 h4 h3 h2 h1 h0) = do
    putWord32be h7 
    putWord32be h6
    putWord32be h5 
    putWord32be h4 
    putWord32be h3 
    putWord32be h2
    putWord32be h1
    putWord32be h0
  get = 
    Hash 
    <$> getWord32be 
    <*> getWord32be 
    <*> getWord32be 
    <*> getWord32be 
    <*> getWord32be 
    <*> getWord32be 
    <*> getWord32be 
    <*> getWord32be

mapHash :: (Word32 -> Word32) -> Hash -> Hash
mapHash f (Hash h7 h6 h5 h4 h3 h2 h1 h0) = 
  Hash (f h7) (f h6) (f h5) (f h4) (f h3) (f h2) (f h1) (f h0)

hashFlipEndian :: Hash -> Hash
hashFlipEndian = mapHash byteSwap32

randomHash :: IO Hash
randomHash = (\(Right hash) -> hash) . runGet get <$> CRT.getRandomBytes 32

hashToBS :: Hash -> ByteString
hashToBS = runPut . put

bsToHash :: ByteString -> Hash
bsToHash = (\(Right hash) -> hash) . runGet get