{-# LANGUAGE OverloadedStrings #-}
module Main where

import Crypto.TL

main :: IO ()
main = 
    do 
        putStrLn "Hashing 'abc' one billion times (~30 seconds)"
        hash <- sha256iterFast 1000000000 $ sha256 "abc"
        print hash