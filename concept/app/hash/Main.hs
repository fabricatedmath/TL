{-# LANGUAGE OverloadedStrings #-}
module Main where

import Crypto.TL
main :: IO ()
main = 
    do 
        putStrLn "Hashing 'abc' one billion times (~30 seconds)"
        print $ hashIter fastMode 1000000000 $ hashDefault "abc"
        