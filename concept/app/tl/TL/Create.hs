module TL.Create where

import Control.Monad.Except (liftIO, runExceptT)

import Options.Applicative

import TL.Util

import Crypto.TL

data Concurrency = Serial | Parallel
    deriving Show

data Create = Create Concurrency Int Int FilePath FilePath
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
  <*> strOption
    ( long "file"
    <> short 'f'
    <> help "File to encrypt with TimeLock"
    <> metavar "FILENAME"
    )
  <*> strOption
    ( long "out"
    <> short 'o'
    <> help "Name of TimeLock file to output"
    <> metavar "FILENAME"
    )

create :: HashFunc -> Create -> IO ()
create hashFunc (Create concurrency numTowers numIters inFile outFile) = do
  putStrLn "Creating TimeLock Archive (TLA) file.."
  putStrLn $ "Creating Chain with " <> show (numIters * numTowers) <> " hashes"
  mchain <- getChainingFunc concurrency hashFunc numTowers numIters
  case mchain of
    Nothing -> putStrLn "Failed to create chain"
    Just chain -> do
      eres <- runExceptT $ encryptTLA inFile outFile chain
      case eres of 
        Left err -> putStrLn err
        Right () -> putStrLn $ "Wrote TLA file to " <> outFile


getChainingFunc :: Concurrency -> HashFunc -> (Int -> Int -> IO (Maybe (Hash, ChainHead)))
getChainingFunc Serial = createChain 
getChainingFunc Parallel = createChainParallel