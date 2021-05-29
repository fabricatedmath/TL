{-# LANGUAGE OverloadedStrings #-}

module TestVectors where

import Crypto.TL.Primitives.Hash
import Crypto.TL.Primitives.HashFunc

hashAbc :: Hash
hashAbc = hashFlipEndian $ hashByteString "abc"

hashAbcGroundTruth :: Hash 
hashAbcGroundTruth = hashFlipEndian $ Hash
    0xba7816bf 0x8f01cfea 0x414140de 0x5dae2223
    0xb00361a3 0x96177a9c 0xb410ff61 0xf20015ad

hashAbcGroundTruthIter1 :: Hash 
hashAbcGroundTruthIter1 = hashFlipEndian $ Hash 
    0x4f8b42c2 0x2dd3729b 0x519ba6f6 0x8d2da7cc
    0x5b2d606d 0x05daed5a 0xd5128cc0 0x3e6c6358

hashAbcGroundTruthIter2 :: Hash 
hashAbcGroundTruthIter2 = hashFlipEndian $ Hash
    0xf2a778f1 0xa6ed3d5b 0xc59a5d79 0x104c598f
    0x3f07093f 0x240ca4e9 0x1333fb09 0xed4f36da