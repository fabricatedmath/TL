module Crypto.TL.Hashing.Util where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (copy, length)
import qualified Data.ByteString.Unsafe as BS (unsafeUseAsCString)
import Data.Int (Int32)
import Foreign.C.String (CString)

import Control.Monad (replicateM)

import Data.Serialize (runGet, get, runPut, put)

import Control.Concurrent

import Control.Monad (replicateM_)

import Data.Maybe (fromMaybe)

import Crypto.TL.Chain
import Crypto.TL.Primitives
import Crypto.TL.Types

availabilityHelper :: Enum a => IO Int32 -> IO a
availabilityHelper = fmap $ toEnum . fromEnum
{-# INLINABLE availabilityHelper #-}

iterateHashHelper :: (Int -> CString -> IO ()) -> HashFunc
iterateHashHelper ffiHashFunction numHashes hash = do
  let bs' = BS.copy $ unHash hash
  BS.unsafeUseAsCString bs' (ffiHashFunction numHashes)
  pure $ Hash bs'
{-# INLINABLE iterateHashHelper #-}

toPacked :: [Hash] -> ByteString
toPacked = runPut . mapM_ put

toUnpacked :: ByteString -> [Hash]
toUnpacked bs = hashes
  where
    Right hashes = runGet (replicateM numHashes get) bs
    numHashes = BS.length bs `div` 32

towerWorker 
  :: HashFunc
  -> Int
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
          let tower = Tower numIters startingHash endingHash
          tower `seq` writeChan towersDone tower
          towerWorker' workPool towersDone

buildTowers
  :: Maybe Int -- core limit 
  -> HashFunc 
  -> Int -- num iters per tower
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