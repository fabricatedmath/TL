module Crypto.TL.Archive 
 ( writeTLA, readTLAHeader, decryptTLA
 , tlaGetChainHead, tlaGetFileName
 ) where

import Conduit (MonadThrow)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Crypto.Cipher.ChaChaPoly1305.Conduit
import qualified Crypto.Random.Types as CRT
import Data.ByteString (ByteString)
import Data.Conduit
import Data.Conduit.Cereal
import Data.Conduit.Combinators (sinkFile, sourceFile)
import Data.Serialize (get, put)

import Crypto.TL.Archive.Header
import Crypto.TL.Primitives.Chain
import Crypto.TL.Primitives.Hash

writeTLA 
  :: FilePath -- inFile
  -> FilePath -- outFile
  -> (Hash, ChainHead)
  -> IO ()
writeTLA inFile outFile (hash, chainHead) =
  runConduitRes $ 
    do
      putHeaderWithChecksumConduit (tlaHeader chainHead inFile)
      sourceFile inFile .| encryptConduit hash 
    .| sinkFile outFile

readTLAHeader
  :: FilePath -- inFile
  -> IO TLAHeader
readTLAHeader inFile = fmap unWithChecksum . runConduitRes $ sourceFile inFile .| sinkGet get

decryptTLA
  :: FilePath -- sourceFile
  -> FilePath -- outFile
  -> Hash
  -> IO ()
decryptTLA inFile outFile hash =
  runConduitRes $ 
    sourceFile inFile 
    .| do
          _ <- getHeaderWithChecksumConduit
          decryptConduit hash
    .| sinkFile outFile

encryptConduit :: (MonadIO m, MonadThrow m) => Hash -> ConduitT ByteString ByteString m ()
encryptConduit hash = do
  nonce <- liftIO $ CRT.getRandomBytes 12
  let hashBS = hashToBS hash
  encrypt nonce hashBS

decryptConduit :: (MonadThrow m) => Hash -> ConduitT ByteString ByteString m ()
decryptConduit = decrypt . hashToBS

getHeaderWithChecksumConduit :: MonadThrow m => ConduitT ByteString o m (WithChecksum TLAHeader)
getHeaderWithChecksumConduit = sinkGet get

putHeaderWithChecksumConduit :: Monad m => TLAHeader -> ConduitT i ByteString m ()
putHeaderWithChecksumConduit = sourcePut . put . WithChecksum