{-# LANGUAGE TupleSections #-}

module TL.Bench where

import Data.Either (rights)
import Data.Time
import Options.Applicative
import Text.Printf

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
bench _ = benchmarkHashFuncs

benchmarkHashFuncs :: IO () 
benchmarkHashFuncs = do
  putStrLn $ "Running benchmarks with " <> show numIters <> " hashes"
  hashFuncs <- rights . map (\(_,name,e) -> either Left (Right . (name,)) e) <$> getHashFuncs
  mapM_ benchmarkHashFunc hashFuncs


benchmarkHashFunc :: (String, HashFunc) -> IO ()
benchmarkHashFunc (name, hashFunc) = do
  start <- getCurrentTime
  hash' <- randomHash >>= hashFunc numIters
  end <- getCurrentTime
  let time = realToFrac $ nominalDiffTimeToSeconds $ diffUTCTime end start :: Double
      hashesPerSecond = fromIntegral numIters / time
  putStrLn $ name <> ": " <> stringifyHashRate hashesPerSecond