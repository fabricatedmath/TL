{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Crypto.Hash.SHA256 as SHA256

import System.Environment (getArgs)

n :: Int
n = 1000000

main :: IO ()
main = 
    do
        [s,ls,ns] <- getArgs

        let [l,n] = map read [ls,ns]
        
        let hashes = take n $ drop (l) $ drop 1 $ iterate SHA256.hash $ BS.pack s
        mapM_ (print . Base16.encode) hashes
