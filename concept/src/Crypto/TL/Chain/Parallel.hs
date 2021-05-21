module Crypto.TL.Chain.Parallel 
  ( createChainParallel
  ) where

import Control.Concurrent

import Control.Monad (replicateM_, replicateM)

import Data.List.NonEmpty (NonEmpty, nonEmpty)

import Crypto.TL.Chain
import Crypto.TL.Primitives
import Crypto.TL.Types

towerWorker 
  :: HashFunc
  -> Chan (Maybe Int) 
  -> Chan Tower 
  -> IO ()
towerWorker hashFunc = towerWorker'
  where 
    towerWorker' workPool towersDone = do
      mwork <- readChan workPool
      case mwork of
        Nothing -> writeChan workPool Nothing
        Just iters -> do
          startingHash <- randomHash
          endingHash <- hashFunc iters startingHash
          let tower = Tower iters startingHash endingHash
          tower `seq` writeChan towersDone tower
          towerWorker' workPool towersDone

buildTowersParallel
  :: HashFunc
  -> Int  -- num Towers
  -> Int -- num iters per tower
  -> IO (Maybe (NonEmpty Tower))
buildTowersParallel hashFunc n i = do
  numCores <- getNumCapabilities
  workPool <- newChan
  replicateM_ n (writeChan workPool $ Just i)
  writeChan workPool Nothing
  towersDone <- newChan
  let numThreadsUsed = min n numCores
  replicateM_ numThreadsUsed $ forkIO (towerWorker hashFunc workPool towersDone)
  towers <- replicateM n $ readChan towersDone
  return $ nonEmpty towers

createChainParallel 
  :: HashFunc
  -> Int -- num towers
  -> Int -- num iters per tower
  -> IO (Maybe (Hash, ChainHead))
createChainParallel hashFunc n i = do
  mtowers <- buildTowersParallel hashFunc n i
  maybe (return Nothing) (fmap Just . foldTowers hashFunc) mtowers