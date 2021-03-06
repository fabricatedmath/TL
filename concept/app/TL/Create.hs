module TL.Create where
  
import Options.Applicative
import System.FilePath.Posix ((<.>), takeBaseName)

import TL.Util

import Crypto.TL
import Crypto.TL.Archive
import Crypto.TL.Util.HashUnits

data Create = Create Int Int FilePath
    deriving Show

createParser :: Parser Create
createParser = Create
  <$> option auto
    ( long "numtowers"
    <> short 'n'
    <> help "Number of towers to build"
    <> metavar "INT"
    )
  <*> option auto
    ( long "iters"
    <> short 'i'
    <> help "Number of hash iterations per tower"
    <> metavar "INT"
    )
  <*> strArgument
    ( help "File to encrypt with TimeLock"
    <> metavar "FILENAME"
    )

create :: Create -> IO ()
create (Create numTowers numIters inFile) = do
  Just (name, hashFunc) <- getBestHashFunc
  Just (nameBulk, (_, bulkHashFunc)) <- getBestBulkHashFunc 
  putStrLn $ "Using Hash Function: " <> name
  putStrLn $ "Using Bulk Hash Function: " <> nameBulk
  putStrLn "Creating TimeLock Archive (TLA) file.."
  putStrLn $ "Creating Chain with " <> stringifyHash (numIters * numTowers)
  mchain <- createChain hashFunc bulkHashFunc numTowers numIters
  case mchain of
    Nothing -> putStrLn "Failed to create chain"
    Just chain -> do
      putStrLn $ "Chain built, writing TimeLock Archive (TLA) file.."
      let outFile = takeBaseName inFile <.> "tla"
      writeTLA inFile outFile chain
      putStrLn $ "Wrote TLA file to " <> outFile