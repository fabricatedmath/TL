module Crypto.TL.Archive 
 ( writeTLA, readTLAHeader, decryptTLA
 , tlaGetChainHead, tlaGetFileName
 ) where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Crypto.Cipher.ChaChaPoly1305.Conduit
import qualified Crypto.Random.Types as CRT
import Data.Conduit
import Data.Conduit.Cereal
import Data.Conduit.Combinators (sinkFile, sourceFile)
import Data.Serialize (Get, get, put)

import Crypto.TL.Archive.Header
import Crypto.TL.Primitives.Chain
import Crypto.TL.Primitives.Hash

writeTLA 
  :: FilePath -- inFile
  -> FilePath -- outFile
  -> (Hash, ChainHead)
  -> IO ()
writeTLA inFile outFile (hash, chainHead) = do
  let header = tlaHeader chainHead inFile
      encryptConduit = do
        nonce <- liftIO $ CRT.getRandomBytes 12
        let hashBS = hashToBS hash
        sourceFile inFile .| encrypt nonce hashBS
  runConduitRes $ (sourcePut (put header) >> encryptConduit) .| sinkFile outFile

readTLAHeader 
  :: FilePath -- inFile
  -> IO TLAHeader
readTLAHeader inFile = runConduitRes $ sourceFile inFile .| sinkGet get

decryptTLA
  :: FilePath -- sourceFile
  -> FilePath -- outFile
  -> Hash
  -> IO ()
decryptTLA inFile outFile hash = do
  let sinkGetHeader = void $ sinkGet (get :: Get TLAHeader)
      decryptConduit = decrypt $ hashToBS hash
  runConduitRes $ sourceFile inFile .| (sinkGetHeader >> decryptConduit) .| sinkFile outFile

