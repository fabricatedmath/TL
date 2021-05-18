{-# LANGUAGE FlexibleContexts #-}

module Crypto.TL.Crypt where

import Control.Monad (void)
import Crypto.Cipher.ChaChaPoly1305.Conduit
import qualified Crypto.Random.Types as CRT
import Data.Conduit
import Data.Conduit.Cereal
import Data.Conduit.Combinators as C
import Data.Serialize

import Crypto.TL.Primitives (Hash(..))
import Crypto.TL.Chain (ChainHead)

encryptTLA
  :: FilePath -- sourceFile
  -> FilePath -- targetFile
  -> (Hash, ChainHead) 
  -> IO ()
encryptTLA inFile outFile ((Hash hashbs), chain) = do
  nonce <- CRT.getRandomBytes 12
  runConduitRes $ (sourcePut (put chain) >> (sourceFile inFile .| encrypt nonce hashbs)) .| sinkFile outFile

decryptTLA
  :: FilePath -- sourceFile
  -> FilePath -- targetFile
  -> Hash
  -> IO ()
decryptTLA inFile outFile (Hash hashbs) = do
  let sinkGetChain = void $ sinkGet (get :: Get ChainHead)
  runConduitRes $ sourceFile inFile .| (sinkGetChain >> decrypt hashbs) .| sinkFile outFile