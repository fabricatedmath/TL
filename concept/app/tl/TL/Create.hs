module TL.Create where

import Control.Monad.Except (liftIO, runExceptT)
import Options.Applicative
import System.FilePath.Posix ((<.>), takeFileName)

import TL.Util

import Crypto.TL

data Concurrency = Serial | Parallel
    deriving Show

data Create = Create Concurrency Int Int FilePath
    deriving Show

createParser :: Parser Create
createParser = Create 
  . fromBool Serial Parallel <$> switch
    ( long "serial"
    <> help "Run tower creation in serial (rather than in parallel)"
    )
  <*> option auto
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

create :: HashFunc -> Create -> IO ()
create hashFunc (Create concurrency numTowers numIters inFile) = do
  putStrLn "Creating TimeLock Archive (TLA) file.."
  putStrLn $ "Creating Chain with " <> show (numIters * numTowers) <> " hashes"
  mchain <- getChainingFunc concurrency hashFunc numTowers numIters
  case mchain of
    Nothing -> putStrLn "Failed to create chain"
    Just chain -> do
      let outFile = takeFileName inFile <.> "tla"
      encryptTLA inFile outFile chain
      putStrLn $ "Wrote TLA file to " <> outFile

getChainingFunc :: Concurrency -> HashFunc -> (Int -> Int -> IO (Maybe (Hash, ChainHead)))
getChainingFunc Serial = createChain 
getChainingFunc Parallel = createChainParallel