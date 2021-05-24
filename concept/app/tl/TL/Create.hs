module TL.Create where

import Control.Monad.Except (liftIO, runExceptT)
import Options.Applicative
import System.FilePath.Posix ((<.>), takeFileName)

import TL.Util

import Crypto.TL

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
  Right bulkHashFunc <- getBulkHashFunc shaModeCuda
  putStrLn $ "Using " <> name <> " Hash Function"
  putStrLn "Creating TimeLock Archive (TLA) file.."
  putStrLn $ "Creating Chain with " <> show (numIters * numTowers) <> " hashes"
  mchain <- createChain hashFunc bulkHashFunc numTowers numIters
  case mchain of
    Nothing -> putStrLn "Failed to create chain"
    Just chain -> do
      let outFile = takeFileName inFile <.> "tla"
      encryptTLA inFile outFile chain
      putStrLn $ "Wrote TLA file to " <> outFile