{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Crypto.TL

import Data.Proxy (Proxy(..))

chainDemo :: forall a. Hashable a => HashMode a -> IO ()
chainDemo mode = 
    do
        let (n,i) = (10,10)
        (hash, chain) <- createChain mode n i
        putStrLn $ "Target Hash in Chain: " ++ show hash
        putStrLn ""
        putStrLn $ "Created Hash Chain: " ++ show chain
        putStrLn ""
        putStrLn $ "Solved Chain: " ++ show (solveChain mode chain)
        putStrLn ""

main :: IO ()
main = 
    do 
        putStrLn "Fast Hashing"
        chainDemo fastMode

        putStrLn "Slow Hashing"
        chainDemo slowMode