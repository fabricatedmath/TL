{-# LANGUAGE TupleSections #-}

module TL.Bench where

import Control.Monad (replicateM)
import Data.Either (rights)
import Data.Time
import Options.Applicative

import Crypto.TL
import Crypto.TL.Util

import TL.Util

data Bench = Bench
    deriving Show

numIters :: Int
numIters = 1000000

benchParser :: Parser Bench
benchParser = pure Bench

bench :: Bench -> IO ()
bench _ = 
  do
    benchmarkHashFuncs
    putStrLn ""
    benchmarkBulkHashFuncs

benchmarkHashFuncs :: IO () 
benchmarkHashFuncs = do
  putStrLn $ "Running benchmarks with " <> stringifyHash numIters
  hashFuncs <- rights . map (\(name,e) -> either Left (Right . (name,)) e) <$> getHashFuncs
  mapM_ benchmarkHashFunc hashFuncs

benchmarkHashFunc :: (String, HashFunc) -> IO ()
benchmarkHashFunc (name, hashFunc) = do
  start <- getCurrentTime
  _hash <- randomHash >>= hashFunc numIters
  end <- getCurrentTime
  let time = realToFrac $ nominalDiffTimeToSeconds $ diffUTCTime end start :: Double
      hashesPerSecond = fromIntegral numIters / time
  putStrLn ""
  putStrLn $ "\t" <> name <> ": " <> stringifyHashRate hashesPerSecond

bulkHashMultiplier :: Int
bulkHashMultiplier = 16

benchmarkBulkHashFuncs :: IO () 
benchmarkBulkHashFuncs = do
  putStrLn $ "Running bulk benchmarks with " <> stringifyHash numIters
  hashFuncs <- rights . map (\(name,e) -> fmap (name,) e) <$> getBulkHashFuncs
  mapM_ benchmarkBulkHashFunc hashFuncs

benchmarkBulkHashFunc :: (String, (Int, BulkHashFunc)) -> IO ()
benchmarkBulkHashFunc (name, (capabilities, bulkHashFunc)) = do
  let threadsSpawned = capabilities * bulkHashMultiplier
  start <- getCurrentTime
  _hash <- replicateM threadsSpawned randomHash >>= bulkHashFunc numIters
  end <- getCurrentTime
  let time = realToFrac $ nominalDiffTimeToSeconds $ diffUTCTime end start :: Double
      hashesPerSecond = fromIntegral (numIters * threadsSpawned) / time
  
  putStrLn $ "\t" <> name <> ": " <> stringifyHashRate hashesPerSecond
  putStrLn ""