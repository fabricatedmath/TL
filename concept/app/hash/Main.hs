{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (void)

import qualified Data.ByteString as BS (length, writeFile)

import Data.Serialize (encode, decode)

import Crypto.TL
import Crypto.TL.Crypt

import Crypto.TL
main :: IO ()
main = 
    do
        Just (hash, chain) <- createChain slowMode 10 10
        let chainBS = encode chain
            chainBSLen = BS.length chainBS
        print chainBSLen
        BS.writeFile "dogs.enc" chainBS

        void $ encrypt "dogs.enc" "dogs.txt" chainBSLen hash

        void $ decrypt "dogs.dec" "dogs.enc" chainBSLen hash
        --putStrLn "Hashing 'abc' one billion times (~30 seconds)"
        --print $ hashIter fastMode 1000000000 $ hashDefault "abc"
        