{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Crypto.TL.Types where

import Control.Monad (replicateM, when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (pack, unpack)
import Data.ByteString.Base16 (encodeBase16')
import Data.Char (chr)
import Data.Proxy (Proxy(..))
import Data.Serialize

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

type HashFunc = Int -> Hash -> Hash

type HashMode a = Proxy a

class HasHashFunc a where
  getHashFunc :: Proxy a -> IO (Either String HashFunc)