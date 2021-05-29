{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Crypto.TL.Types 
  ( Hash(..), Checksum(..), EncryptedHash(..)
  , HashMode, HashFunc, HasHashFunc(..)
  , Bulk, HasBulkHashFunc(..), BulkHashFunc
  , Tower(..), ShortHash(..)
  , hashFlipEndian, shortHashFlipEndian
  ) where

import Control.Concurrent
import Control.Monad (replicateM, replicateM_)

import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(..))
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

mapHash :: (Word32 -> Word32) -> Hash -> Hash
mapHash f (Hash h7 h6 h5 h4 h3 h2 h1 h0) = 
  Hash (f h7) (f h6) (f h5) (f h4) (f h3) (f h2) (f h1) (f h0)

hashFlipEndian :: Hash -> Hash
hashFlipEndian = mapHash byteSwap32

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
  { towerStart :: !Hash
  , towerEnd :: !Hash
  } deriving (Eq, Show)

data Bulk a

type BulkHashFunc = Int -> [Hash] -> IO [Tower]

class HasBulkHashFunc a where
  getBulkHashFunc :: HashMode a -> IO (Either String (Int, BulkHashFunc))

instance forall a. HasHashFunc a => HasBulkHashFunc (Bulk a) where
  getBulkHashFunc _ = do
    ehashFunc <- getHashFunc (Proxy :: Proxy a)
    numCores <- getNumCapabilities
    return $ (numCores,) . buildTowers Nothing <$> ehashFunc

towerWorker 
  :: HashFunc
  -> Int -- | numIters
  -> Chan (Maybe Hash) 
  -> Chan Tower
  -> IO ()
towerWorker hashFunc numIters = towerWorker'
  where 
    towerWorker' workPool towersDone = do
      mwork <- readChan workPool
      case mwork of
        Nothing -> writeChan workPool Nothing
        Just startingHash -> do
          endingHash <- hashFunc numIters startingHash
          let tower = Tower startingHash endingHash
          tower `seq` writeChan towersDone tower
          towerWorker' workPool towersDone

buildTowers
  :: Maybe Int -- core limit 
  -> HashFunc 
  -> Int -- | numIters
  -> [Hash]
  -> IO [Tower]
buildTowers mlimit hashFunc numIters startingHashes = do
  let numTowers = length startingHashes
  numCores <- getNumCapabilities
  workPool <- newChan
  mapM_ (writeChan workPool . Just) startingHashes
  writeChan workPool Nothing
  towersDone <- newChan
  let numThreadsUsed = min (fromMaybe maxBound mlimit) $ min numTowers numCores
  replicateM_ numThreadsUsed $ forkIO (towerWorker hashFunc numIters workPool towersDone)
  replicateM numTowers $ readChan towersDone

data ShortHash = ShortHash {-# UNPACK #-} !Word32 !Word32
  deriving Eq

instance Show ShortHash where
  show shortHash = printf formatString h7 h6
    where (ShortHash h7 h6) = shortHashFlipEndian shortHash
          formatString = concat $ replicate 2 "%08x"

shortHashFlipEndian :: ShortHash -> ShortHash
shortHashFlipEndian (ShortHash h7 h6) = ShortHash (byteSwap32 h7) (byteSwap32 h6)

instance Serialize ShortHash where
  put (ShortHash h7 h6) = do
    putWord32be h7
    putWord32be h6
  get = 
    ShortHash <$> getWord32be <*> getWord32be 