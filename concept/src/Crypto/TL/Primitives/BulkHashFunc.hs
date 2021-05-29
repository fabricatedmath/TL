{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Crypto.TL.Primitives.BulkHashFunc 
  ( Bulk, BulkHashFunc, HasBulkHashFunc(..)
  , toPacked, toUnpacked
  ) where

import Control.Concurrent (Chan, forkIO, getNumCapabilities, newChan, readChan, writeChan)
import Control.Monad (replicateM, replicateM_)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (length)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(..))
import Data.Serialize

import Crypto.TL.Primitives.Hash
import Crypto.TL.Primitives.HashFunc
import Crypto.TL.Primitives.Tower

data Bulk a

type BulkHashFunc = Int -> [Hash] -> IO [Tower]

class HasBulkHashFunc a where
  getBulkHashFunc :: HashMode a -> IO (Either String (Int, BulkHashFunc))

instance forall a. HasHashFunc a => HasBulkHashFunc (Bulk a) where
  getBulkHashFunc _ = do
    ehashFunc <- getHashFunc (Proxy :: Proxy a)
    numCores <- getNumCapabilities
    return $ (numCores,) . buildTowers Nothing <$> ehashFunc

toPacked :: [Hash] -> ByteString
toPacked = runPut . mapM_ put

toUnpacked :: ByteString -> [Hash]
toUnpacked bs = hashes
  where
    Right hashes = runGet (replicateM numHashes get) bs
    numHashes = BS.length bs `div` 32

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