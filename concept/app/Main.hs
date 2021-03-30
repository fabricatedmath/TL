module Main where

import Crypto.TL

main :: IO ()
main = 
    do  let (n,i) = (10,10)
        Right (hash, chain) <- createChain n i
        putStrLn $ "Target Hash in Chain: " ++ show hash
        putStrLn ""
        putStrLn $ "Created Hash Chain: " ++ show chain
        putStrLn ""
        putStrLn $ "Solved Chain: " ++ show (solveChain chain)
