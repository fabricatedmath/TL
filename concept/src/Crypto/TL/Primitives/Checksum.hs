{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Crypto.TL.Primitives.Checksum 
  ( Checksum, calcChecksum, verifyChecksum, incrementHash
  , fileToChecksumMap, fileToHashLength
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Serialize
import Data.Text (Text)
import qualified Data.Text as T

import Data.Conduit
import qualified Data.Conduit.Combinators as C
import Data.Word (Word32)

import Crypto.TL.Primitives.Hash
import Crypto.TL.Primitives.HashFunc

newtype Checksum = 
    Checksum 
    { unChecksum :: Hash
    } deriving (Eq, Ord, Serialize)

instance Show Checksum where
  show = show . unChecksum

mapAccumR :: (acc -> Word32 -> (acc, Word32)) -> acc -> Hash -> (acc, Hash)
mapAccumR f acc (Hash h7 h6 h5 h4 h3 h2 h1 h0) = 
  (acc7, Hash h7' h6' h5' h4' h3' h2' h1' h0')
  where 
    (acc0, h0') = f acc h0
    (acc1, h1') = f acc0 h1
    (acc2, h2') = f acc1 h2
    (acc3, h3') = f acc2 h3
    (acc4, h4') = f acc3 h4
    (acc5, h5') = f acc4 h5
    (acc6, h6') = f acc5 h6
    (acc7, h7') = f acc6 h7

incrementHash :: Hash -> Hash
incrementHash = snd . mapAccumR f True
  where f False w = (False, w) 
        f True w = case w + 1 of 
                    0 -> (True, 0)
                    w' -> (False, w')

-- Add 1 to each byte (with overflow) and then hash to yield our checkpoint
calcChecksum :: HashFunc -> Hash -> IO Checksum
calcChecksum hashFunc = fmap Checksum . hashOnce hashFunc . incrementHash

verifyChecksum :: HashFunc -> Checksum -> Hash -> IO Bool
verifyChecksum hashFunc checksum = fmap (checksum ==) . calcChecksum hashFunc

hexPredicate :: Char -> Bool
hexPredicate c = 
  ('0' <= c && c <= '9') || 
  ('a' <= c && c <= 'f') || 
  ('A' <= c && c <= 'F')

fileToHashLength :: HashFunc -> FilePath -> IO Int
fileToHashLength hashFunc fp = runConduitRes $ C.sourceFile fp .| C.decodeUtf8 .| textToHashesConduit .| C.length

fileToChecksumMap :: HashFunc -> FilePath -> IO (Map Checksum Hash)
fileToChecksumMap hashFunc fp = runConduitRes $ C.sourceFile fp .| C.decodeUtf8 .| textToHashesConduit .| checksumMapConduit hashFunc

checksumMapConduit :: MonadIO m => HashFunc -> ConduitT Hash Void m (Map Checksum Hash)
checksumMapConduit hashFunc = go Map.empty
  where 
    go m = do
      !mhash <- await
      case mhash of
        Just !hash -> do
          !checksum <- liftIO $ calcChecksum hashFunc hash
          let !m' = Map.insert checksum hash m
          m' `seq` go m'
        Nothing -> return m

textToHashesConduit :: Monad m => ConduitT Text Hash m ()
textToHashesConduit = textToHashesConduit' ""

textToHashesConduit' :: Monad m => Text -> ConduitT Text Hash m ()
textToHashesConduit' prevT = do
  !mtext <- await
  case mtext of
    Nothing -> return ()
    Just !currT -> do
      !prevT' <- processTextToHashes prevT currT
      prevT' `seq` textToHashesConduit' prevT'

processTextToHashes :: Monad m => Text -> Text -> ConduitT i Hash m Text
processTextToHashes prevT currT = 
  case T.uncons currT of
    Nothing -> return prevT
    Just (!c, !currT') -> do
      !prevT' <-
        case hexPredicate c of
          True -> processText $ T.snoc prevT c
          False -> return ""
      prevT' `seq` currT' `seq` processTextToHashes prevT' currT'

processText :: Monad m => Text -> ConduitT i Hash m Text
processText t 
  | T.length t == 64 = do
      let Just !hash = textToHash t
      hash `seq` yield hash
      return ""
  | otherwise = return t