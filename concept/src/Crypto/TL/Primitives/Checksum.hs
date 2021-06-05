{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Crypto.TL.Primitives.Checksum 
  ( Checksum, calcChecksum, verifyChecksum, incrementHash
  ) where

import Data.Serialize
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