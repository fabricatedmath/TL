{-# LANGUAGE OverloadedStrings #-}
module Main where

import Crypto.TL

hashFast :: Int -> Hash Fast -> Hash Fast
hashFast = hashIter

main :: IO ()
main = 
    do 
        putStrLn "Hashing 'abc' one billion times (~30 seconds)"
        print $ hashFast 1000000000 $ hashDefault "abc"
        