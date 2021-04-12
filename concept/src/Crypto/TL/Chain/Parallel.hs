module Crypto.TL.Chain.Parallel 
    ( createChainParallel
    ) where

import Control.Concurrent

import Control.Monad (replicateM_, replicateM)

import Data.List.NonEmpty (NonEmpty, nonEmpty)

import Crypto.TL.Chain
import Crypto.TL.Primitives

towerWorker 
    :: Hashable a 
    => HashMode a 
    -> Chan (Maybe Int) 
    -> Chan Tower 
    -> IO ()
towerWorker mode workPool towersDone = 
    do
        mwork <- readChan workPool
        case mwork of
            Nothing -> writeChan workPool Nothing
            Just iters -> 
                do
                    startingHash <- randomHash
                    let endingHash = hashIter mode iters startingHash
                        tower = Tower iters startingHash endingHash
                    tower `seq` writeChan towersDone tower
                    towerWorker mode workPool towersDone

buildTowersParallel
    :: Hashable a 
    => HashMode a 
    -> Int  -- num Towers
    -> Int -- num iters per tower
    -> IO (Maybe (NonEmpty Tower))
buildTowersParallel mode n i = 
    do
        numCores <- getNumCapabilities
        workPool <- newChan
        replicateM_ n (writeChan workPool $ Just i)
        writeChan workPool Nothing
        towersDone <- newChan
        let numThreadsUsed = min n numCores
        replicateM_ numThreadsUsed $ forkIO (towerWorker mode workPool towersDone)
        towers <- replicateM n $ readChan towersDone
        return $ nonEmpty towers

createChainParallel 
    :: Hashable a 
    => HashMode a 
    -> Int -- num towers
    -> Int -- num iters per tower
    -> IO (Maybe (Hash, ChainHead))
createChainParallel mode n i = 
    do
        mtowers <- buildTowersParallel mode n i
        return $ foldTowers mode <$> mtowers
