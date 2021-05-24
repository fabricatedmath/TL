{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}

module TL.Util where

import Data.Either (rights)
import Data.Foldable (asum)
import Data.Maybe (catMaybes, listToMaybe)
import Options.Applicative

import Crypto.TL

fromBool :: a -> a -> Bool -> a
fromBool a1 _ True = a1
fromBool _ a2 False = a2

-- TODO: return message to display later about missing hashing impls
hashFuncParser :: IO (Parser HashFunc)
hashFuncParser = do
  bestHashFunc <- getBestHashFunc
  getHashFuncs >>= fmap (asum . catMaybes) . mapM makeMode
  where
    makeMode :: (Char, String, Either String HashFunc) -> IO (Maybe (Parser HashFunc))
    makeMode (c,s,Right hashFunc) = return $ Just $ flag' hashFunc (short c <> help s)
    makeMode (_,s,Left message) = putStrLn (s <> " " <> message) >> return Nothing

getBestHashFunc :: IO (Maybe (String, HashFunc))
getBestHashFunc = do
  hashFuncs <- rights . map (\(f,name,efunc) -> either Left (Right . (name,)) efunc) <$> getHashFuncs
  return $ listToMaybe hashFuncs

data HashBox = forall a. HasHashFunc a => HB (HashMode a)

getHashFuncs :: IO [(Char, String, Either String HashFunc)]
getHashFuncs = mapM f
  [ ('x', "x86", HB shaModeX86)
  , ('a', "Arm", HB shaModeArm)
  , ('g', "Generic", HB shaModeGeneric)
  , ('n', "Native", HB shaModeNative)
  ]
  where 
    f (c,s,HB a) = do
      ehashFunc <- getHashFunc a
      return (c,s,ehashFunc)