{-# LANGUAGE FlexibleContexts #-}

module TL.Solve where

import Control.Monad.Except (ExceptT(..), MonadError(..), liftIO, runExceptT)
import Control.Monad.IO.Class
import Control.Monad.State

import qualified Data.ByteString as BS (readFile)
import qualified Data.Serialize as S

import Options.Applicative

import System.FilePath.Posix (dropExtension)

import Crypto.TL

import TL.Util

data Solve = Solve Bool FilePath
  deriving Show

solveParser :: Parser Solve
solveParser = Solve 
  <$> switch 
  ( long "silent"
  <> help "Suppress Printed Info"
  )
  <*> strArgument
  ( help "TimeLock file to solve"
  <> metavar "FILENAME"
  )

solve :: Solve -> IO ()
solve (Solve verbose inFile) = do
  echain <- S.runGet S.get <$> BS.readFile inFile
  Just (name, hashFunc) <- getBestHashFunc
  putStrLn $ "Using " <> name <> " Hash Function"
  case echain of
    Left err -> putStrLn err
    Right chain -> do
      let numTowers = numTowersInChain chain
          numHashes = numHashesInChain chain
      putStrLn $ "Solving chain with " <> show numTowers <> " towers and " <> show numHashes <> " total hashes" <> "\n"
      let outFile = dropExtension inFile 
      ehash <- flip evalStateT 0 $ runExceptT $ do
        hash <- getSolvingFunc numTowers verbose hashFunc chain
        liftIO $ decryptTLA inFile outFile hash
        pure hash
      case ehash of 
        Left err -> putStrLn err
        Right hash -> do
          when verbose $ putStrLn $ "Chain solved with hash: " <> show hash <> "\n"
          putStrLn $ "Wrote decrypted file to " <> outFile

getSolvingFunc 
  :: (MonadError String m, MonadIO m, MonadState Int m) 
  => Int -- | Num towers
  -> Bool -- | Suppress Reporting
  -> HashFunc -- | Hashing Function to Use
  -> (ChainHead -> m Hash)
getSolvingFunc _ True = solveChain
getSolvingFunc numTowers False = solveChain' (startReporter numTowers) solveReporter

startReporter :: (MonadIO m, MonadState Int m) => Int -> (Int -> m ())
startReporter numTowers i = do
    towerNum <- gets succ
    put towerNum
    liftIO $ putStrLn $ "Solving tower " <> show towerNum <> " of " <> show numTowers <> " with " <> show i <> " hashes.."

solveReporter :: (MonadIO m, MonadState Int m) => (Hash -> m ())
solveReporter hash = do
  towerNum <- get
  liftIO $ putStrLn $ "Tower " <> show towerNum <> " solved and verified with hash: " <> show hash <> "\n"