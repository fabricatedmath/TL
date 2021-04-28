{-# LANGUAGE FlexibleContexts #-}

module TL.Solve where

import Control.Monad.Except (ExceptT(..), MonadError(..), liftIO, runExceptT)
import Control.Monad.IO.Class
import Control.Monad.State

import qualified Data.ByteString as BS (readFile)
import qualified Data.Serialize as S

import Options.Applicative

import Crypto.TL

import TL.Util

data Solve = Solve Bool FilePath FilePath
  deriving Show

solveParser :: Parser Solve
solveParser = Solve 
  <$> switch 
  ( long "verbose"
  <> short 'v'
  <> help "Print info"
  )
  <*> strOption
  ( long "file"
  <> short 'f'
  <> help "TimeLock file to solve"
  <> metavar "FILENAME"
  )
  <*> strOption
  ( long "out"
  <> short 'o'
  <> help "Name of file to output"
  <> metavar "FILENAME"
  )

solve :: Mode -> Solve -> IO ()
solve mode (Solve verbose inFile outFile) = do
  echain <- S.runGet S.get <$> BS.readFile inFile
  case echain of
    Left err -> putStrLn err
    Right chain -> do
      let numTowers = numTowersInChain chain
          numHashes = numHashesInChain chain
      putStrLn $ "Solving chain with " <> show numTowers <> " towers and " <> show numHashes <> " total hashes" <> "\n"
      ehash <- flip evalStateT 0 $ runExceptT $ do
        hash <- getSolvingFunc numTowers verbose mode chain
        decryptTLA inFile outFile hash
        pure hash
      case ehash of 
        Left err -> putStrLn err
        Right hash -> do
          when verbose $ putStrLn $ "Chain solved with hash: " <> show hash <> "\n"
          putStrLn $ "Wrote decrypted file to " <> outFile

getSolvingFunc :: (MonadError String m, MonadIO m, MonadState Int m) => Int -> Bool -> Mode -> (ChainHead -> m Hash)
getSolvingFunc _ False Slow = solveChain slowMode
getSolvingFunc _ False Fast = solveChain fastMode
getSolvingFunc numTowers True Slow = solveChain' (startReporter numTowers) solveReporter slowMode
getSolvingFunc numTowers True Fast = solveChain' (startReporter numTowers) solveReporter fastMode

startReporter :: (MonadIO m, MonadState Int m) => Int -> (Int -> m ())
startReporter numTowers i = do
    towerNum <- succ <$> get
    put $ towerNum
    liftIO $ putStrLn $ "Solving tower " <> show towerNum <> " of " <> show numTowers <> " with " <> show i <> " hashes.."

solveReporter :: (MonadIO m, MonadState Int m) => (Hash -> m ())
solveReporter hash = do
  towerNum <- get
  liftIO $ putStrLn $ "Tower " <> show towerNum <> " solved and verified with hash: " <> show hash <> "\n"
