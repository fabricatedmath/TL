{-# LANGUAGE OverloadedStrings #-}

module TestVectors where

import qualified Crypto.Hash as Hash

import Crypto.TL.Primitives (Hash(..), hashFlipEndian)

import qualified Data.ByteArray as ByteArray

import Data.ByteString (ByteString)
import Data.ByteString.Base16 (decodeBase16)

hashAbc :: Hash
hashAbc = hashFlipEndian $ Hash hash
    where hash = ByteArray.convert $ Hash.hashWith Hash.SHA256 ("abc" :: ByteString)

hashAbcGroundTruth :: Hash 
hashAbcGroundTruth = hashFlipEndian $ Hash hash
    where Right hash = decodeBase16 "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"

hashAbcGroundTruthIter1 :: Hash 
hashAbcGroundTruthIter1 = hashFlipEndian $ Hash hash
    where Right hash = decodeBase16 "4f8b42c22dd3729b519ba6f68d2da7cc5b2d606d05daed5ad5128cc03e6c6358"

hashAbcGroundTruthIter2 :: Hash 
hashAbcGroundTruthIter2 = hashFlipEndian $ Hash hash
    where Right hash = decodeBase16 "f2a778f1a6ed3d5bc59a5d79104c598f3f07093f240ca4e91333fb09ed4f36da"
