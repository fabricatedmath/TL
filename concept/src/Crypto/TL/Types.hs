{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Crypto.TL.Types where

import Control.Monad (replicateM)
import Data.ByteString (ByteString)
import Data.ByteString.Base16 (encodeBase16')
import Data.Char (chr)
import Data.Proxy (Proxy(..))
import Data.Serialize

newtype Hash = 
  Hash 
  { unHash :: ByteString
  } deriving Eq

fromRight :: Either a b -> b
fromRight (Right b) = b
fromRight (Left _) = error "Failed to convert value to Right"

instance Show Hash where
  show = map (chr . fromEnum) . fromRight . runGet (replicateM 32 getWord8) . encodeBase16' . unHash
  
instance Serialize Hash where
  put = mapM_ putWord32be . fromRight . runGet (replicateM 8 getWord32be) . unHash
  get = Hash . runPut . mapM_ putWord32be <$> replicateM 8 getWord32be

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