{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Crypto.TL.Types 
  ( Hash(..), Checksum(..), EncryptedHash(..)
  , HashMode, HashFunc, HasHashFunc(..)
  , Bulk, HasBulkHashFunc(..), BulkHashFunc
  , Tower(..)
  , fromRight
  ) where

import Control.Concurrent
import Control.Monad (replicateM, replicateM_)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (unpack)
import Data.ByteString.Base16 (encodeBase16')
import Data.Char (chr)
import Data.Maybe (fromMaybe)
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
  show = map (chr . fromEnum) . BS.unpack . encodeBase16' . unHash
  
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
  { towerStart :: !Hash
  , towerEnd :: !Hash
  } deriving Show

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