{-# LANGUAGE DeriveGeneric #-}

module Crypto.TL.Primitives.Hash
  ( Hash(..)
  , hashFlipEndian, randomHash, bsToHash, hashToBS
  , stringToHash, textToHash
  ) where

import qualified Crypto.Random.Types as CRT (getRandomBytes)

import Data.Text (Text)
import qualified Data.Text as T

import Control.DeepSeq
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Base16 (decodeBase16)
import Data.Serialize
import Data.Word (Word32, byteSwap32)
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import GHC.Generics (Generic)
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
  deriving (Eq, Generic, Ord)

instance Storable Hash where
  sizeOf _ = 32
  alignment _ = 32
  peek ptr = do
    let 
      wp1 = castPtr ptr
      wp2 = advancePtr wp1 1
      wp3 = advancePtr wp2 1
      wp4 = advancePtr wp3 1
      wp5 = advancePtr wp4 1
      wp6 = advancePtr wp5 1
      wp7 = advancePtr wp6 1
      wp8 = advancePtr wp7 1 
    Hash 
      <$> peek wp1 <*> peek wp2 <*> peek wp3 <*> peek wp4
      <*> peek wp5 <*> peek wp6 <*> peek wp7 <*> peek wp8

  poke ptr (Hash w1 w2 w3 w4 w5 w6 w7 w8) = do
    let 
      wp1 = castPtr ptr
      wp2 = advancePtr wp1 1
      wp3 = advancePtr wp2 1
      wp4 = advancePtr wp3 1
      wp5 = advancePtr wp4 1
      wp6 = advancePtr wp5 1
      wp7 = advancePtr wp6 1
      wp8 = advancePtr wp7 1
    poke wp1 w1
    poke wp2 w2
    poke wp3 w3
    poke wp4 w4
    poke wp5 w5
    poke wp6 w6
    poke wp7 w7
    poke wp8 w8

instance NFData Hash 

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

textToHash :: Text -> Maybe Hash
textToHash = stringToHash . T.unpack

stringToHash :: String -> Maybe Hash
stringToHash s = fmap hashFlipEndian $ 
  case decodeBase16 . BS.pack . map (fromIntegral . fromEnum) $ s of
    Left _ -> Nothing
    Right bs -> either (const Nothing) Just $ decode bs

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