{-# LANGUAGE FlexibleContexts #-}

module TL.Solve where

import Control.Monad.Except (MonadError(..), runExceptT)
import Control.Monad.IO.Class
import Control.Monad.State

import qualified Data.ByteString as BS (readFile)
import qualified Data.Serialize as S

import Options.Applicative

import System.FilePath.Posix (dropExtension)

import Crypto.TL
import Crypto.TL.Archive
import Crypto.TL.Util

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
solve (Solve silent inFile) = do
  header <- readTLAHeader inFile
  Just (name, hashFunc) <- getBestHashFunc
  putStrLn $ "Using " <> name <> " Hash Function" <> "\n"
  let chain = tlaGetChainHead header
      numTowers = numTowersInChain chain
      numHashes = numHashesInChain chain
  unless silent $ putStrLn $ "Solving chain with " <> show numTowers <> " towers and " <> stringifyHash numHashes <> "\n"
  let outFile = tlaGetFileName header
  result <- flip evalStateT 0 $ runExceptT $ do
    hash <- getSolvingFunc numTowers silent hashFunc chain
    liftIO $ do
      unless silent $ putStrLn $ "Chain solved with hash: " <> show hash <> "\n"
      unless silent $ putStrLn $ "Decrypting file.." <> "\n"
      decryptTLA inFile outFile hash
      unless silent $ putStrLn $ "Wrote decrypted file to " <> outFile
  case result of 
    Left err -> putStrLn err
    Right _hash -> return ()

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
    liftIO $ putStrLn $ "Solving tower " <> show towerNum <> " of " <> show numTowers <> " with " <> stringifyHash i <> ".."

solveReporter :: (MonadIO m, MonadState Int m) => (Hash -> m ())
solveReporter hash = do
  towerNum <- get
  liftIO $ putStrLn $ "Tower " <> show towerNum <> " solved and verified with hash: " <> show hash <> "\n"