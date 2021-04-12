{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (void)
import Control.Monad.Except (liftIO, runExceptT)

import qualified Data.ByteString as BS (length, writeFile)

import Data.Serialize (encode, decode)

import Crypto.TL
import Crypto.TL.Crypt

import Crypto.TL

main :: IO ()
main = 
    do
        Just (hash, chain) <- createChain slowMode 10 10

        v <- runExceptT $ do
            encryptTLA "dogs.enc" "dogs.txt" (hash, chain)
            decryptTLA "dogs.dec" "dogs.enc" hash
            liftIO $ putStrLn "Sucessfully decrypted TLA file"

        print v
