{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Crypto.TL

import Data.Proxy (Proxy(..))

chainDemo :: forall a. Hashable a => Proxy a -> IO ()
chainDemo _ = 
    do
        let (n,i) = (10,10)
        (hash, chain) <- createChain n i :: IO (Hash a, ChainHead a)
        putStrLn $ "Target Hash in Chain: " ++ show hash
        putStrLn ""
        putStrLn $ "Created Hash Chain: " ++ show chain
        putStrLn ""
        putStrLn $ "Solved Chain: " ++ show (solveChain chain)
        putStrLn ""

main :: IO ()
main = 
    do 
        let fastProxy = Proxy :: Proxy Fast
            slowProxy = Proxy :: Proxy Slow
        putStrLn "Fast Hashing"
        chainDemo fastProxy

        putStrLn "Slow Hashing"
        chainDemo slowProxy