module Crypto.TL.Crypt where

import Control.Monad (void)
import Crypto.Cipher.ChaChaPoly1305.Conduit
import qualified Crypto.Random.Types as CRT
import Data.Conduit
import Data.Conduit.Cereal
import Data.Conduit.Combinators as C
import Data.Serialize

import Crypto.TL.Chain (ChainHead)
import Crypto.TL.Primitives (Hash(..), hashToBS)

encryptTLA
  :: FilePath -- sourceFile
  -> FilePath -- targetFile
  -> (Hash, ChainHead) 
  -> IO ()
encryptTLA inFile outFile (hash, chain) = do
  nonce <- CRT.getRandomBytes 12
  let hashBS = hashToBS hash
  runConduitRes $ (sourcePut (put chain) >> (sourceFile inFile .| encrypt nonce hashBS)) .| sinkFile outFile

decryptTLA
  :: FilePath -- sourceFile
  -> FilePath -- targetFile
  -> Hash
  -> IO ()
decryptTLA inFile outFile hash = do
  let sinkGetChain = void $ sinkGet (get :: Get ChainHead)
      hashBS = hashToBS hash
  runConduitRes $ sourceFile inFile .| (sinkGetChain >> decrypt hashBS) .| sinkFile outFile