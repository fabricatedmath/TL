{-# LANGUAGE RankNTypes #-}

module TL.Util where

import Data.Foldable (asum)
import Data.Maybe (catMaybes)
import Options.Applicative

import Crypto.TL (HashFunc, getHashFuncs)

fromBool :: a -> a -> Bool -> a
fromBool a1 _ True = a1
fromBool _ a2 False = a2

-- TODO: return message to display later about missing hashing impls
hashFuncParser :: IO (Parser HashFunc)
hashFuncParser = getHashFuncs >>= fmap (asum . catMaybes) . mapM makeMode
  where
    makeMode :: (Char, String, Either String HashFunc) -> IO (Maybe (Parser HashFunc))
    makeMode (c,s,Right hashFunc) = return $ Just $ flag' hashFunc (short c <> help s)
    makeMode (_,s,Left message) = putStrLn (s <> " " <> message) >> return Nothing
