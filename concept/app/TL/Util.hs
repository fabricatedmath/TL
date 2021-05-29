{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}

module TL.Util 
  ( getBestHashFunc, getBestBulkHashFunc
  , getHashFuncs, getBulkHashFuncs
  ) where

import Data.Either (rights)
import Data.Maybe (listToMaybe)

import Crypto.TL

getBestHashFunc :: IO (Maybe (String, HashFunc))
getBestHashFunc = do
  hashFuncs <- rights . map (\(name,efunc) -> fmap (name,) efunc) <$> getHashFuncs
  return $ listToMaybe hashFuncs

data HashBox = forall a. HasHashFunc a => HB (HashMode a)

getHashFuncs :: IO [(String, Either String HashFunc)]
getHashFuncs = mapM f
  [ ("x86 with SSE4.1 and SHA CPU Extensions", HB shaModeX86)
  , ("Arm with SHA2 CPU Extensions", HB shaModeArm)
  , ("Generic (No CPU Extension assistance)", HB shaModeGeneric)
  ]
  where 
    f (s,HB a) = do
      ehashFunc <- getHashFunc a
      return (s,ehashFunc)

getBestBulkHashFunc :: IO (Maybe (String, (Int, BulkHashFunc)))
getBestBulkHashFunc = do
  hashFuncs <- rights . map (\(name,efunc) -> fmap (name,) efunc) <$> getBulkHashFuncs
  return $ listToMaybe hashFuncs

data HashBoxBulk = forall a. HasBulkHashFunc a => HBB (HashMode a)

getBulkHashFuncs :: IO [(String, Either String (Int, BulkHashFunc))]
getBulkHashFuncs = mapM f
  [ ("NVIDIA Cuda", HBB shaModeCuda)
  , ("x86 with SSE4.1 and SHA CPU Extensions", HBB shaModeBulkX86)
  , ("Arm with SHA2 CPU Extensions", HBB shaModeBulkArm)
  , ("Generic (No CPU Extension assistance)", HBB shaModeBulkGeneric)
  ]
  where 
    f (s,HBB a) = do
      ehashFunc <- getBulkHashFunc a
      return (s,ehashFunc)