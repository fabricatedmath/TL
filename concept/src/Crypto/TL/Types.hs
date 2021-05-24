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

type HashMode a = Proxy a

type HashFunc = Int -> Hash -> IO Hash

class HasHashFunc a where
  getHashFunc :: HashMode a -> IO (Either String HashFunc)

data Tower = 
  Tower
  { towerSize :: !Int
  , towerStart :: !Hash
  , towerEnd :: !Hash
  } deriving Show

type BulkHashFunc = Int -> [Hash] -> IO [Tower]

class HasBulkHashFunc a where
  getBulkHashFunc :: HashMode a -> IO (Either String BulkHashFunc)