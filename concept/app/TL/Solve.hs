{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module TL.Solve where

import Control.DeepSeq
import Control.Monad.Except (MonadError(..), runExceptT)
import Control.Monad.IO.Class
import Control.Monad.State
import qualified Data.ByteString as BS (readFile)
import Data.Maybe (maybeToList)
import qualified Data.Serialize as S
import qualified Data.Map.Strict as Map
import Options.Applicative
import System.Exit (exitFailure, exitSuccess)
import System.FilePath.Posix (dropExtension)

import Crypto.TL
import Crypto.TL.Archive
import Crypto.TL.Util

import TL.Util

data Solve = Solve Bool (Maybe Hash) (Maybe FilePath) FilePath
  deriving Show

solveParser :: Parser Solve
solveParser = Solve 
  <$> switch 
  ( long "silent"
  <> help "Suppress Printed Info"
  )
  <*> option (maybeReader $ fmap Just . stringToHash)
  ( long "resume"
  <> value Nothing
  <> help "Resume solving with the given hash in hex form"
  )
  <*> option (maybeReader $ Just . Just)
  ( long "resume-file"
  <> value Nothing
  <> help "Resume solving with any possible hashes found in supplied file"
  <> metavar "FILENAME"
  )
  <*> strArgument
  ( help "TimeLock file to solve"
  <> metavar "FILENAME"
  )

trySkippingHashes :: HashFunc -> Maybe Hash -> Maybe FilePath -> ChainHead -> FilePath -> FilePath -> IO (Int, ChainHead)
trySkippingHashes _ Nothing Nothing chainHead _ _ = return (0, chainHead)
trySkippingHashes hashFunc _ mresumeFile chainHead inFile outFile = do
  putStrLn "Getting hashes.."
  hashLength <- maybe (return 0) (fileToHashLength hashFunc) mresumeFile
  putStrLn $ show hashLength
  !checksumMap <- maybe (return Map.empty) (fileToChecksumMap hashFunc) mresumeFile
  checksumMap `seq` putStrLn "Got Hashes"
  mskippedChain <- resumeChainFromMap checksumMap chainHead
  case mskippedChain of
    (0, _) -> do
      putStrLn $ "Failed to find any supplied hashes in chain, exiting.."
      exitFailure
    (_skippedTowers, Left solutionHash) -> do
      putStrLn $ "A supplied hash was found to be the solving hash for the chain! Decrypting.." <> "\n"
      putStrLn $ "Solution hash is: " <> show solutionHash <> "\n"
      decryptTLA inFile outFile solutionHash
      putStrLn $ "Wrote decrypted file to " <> outFile
      exitSuccess
    (skippedTowers, Right skippedChain) -> do
      putStrLn $ "A supplied hash was found in the chain! Skipping " <> show skippedTowers <> " towers.." <> "\n"
      return (skippedTowers, skippedChain)

solve :: Solve -> IO ()
solve (Solve silent mresume mresumeFile inFile) = do
  header <- readTLAHeader inFile
  Just (name, hashFunc) <- getBestHashFunc
  putStrLn $ "Using " <> name <> " Hash Function" <> "\n"
  let !chain = tlaGetChainHead header
      numTowers = numTowersInChain chain
      numHashes = numHashesInChain chain
  chain `seq` (unless silent $ putStrLn $ "Solving chain with " <> show numTowers <> " towers and " <> stringifyHash numHashes <> "\n")
  let outFile = tlaGetFileName header
  (startingTower, chain') <- trySkippingHashes hashFunc mresume mresumeFile chain inFile outFile
  result <- flip evalStateT startingTower $ runExceptT $ do
    hash <- getSolvingFunc numTowers silent hashFunc chain'
    liftIO $ do
      unless silent $ putStrLn $ "Chain solved with hash: " <> show hash <> "\n"
      unless silent $ putStrLn $ "Decrypting file.." <> "\n"
      decryptTLA inFile outFile hash
      unless silent $ putStrLn $ "Wrote decrypted file to " <> outFile
  case result of 
    Left err -> do
      putStrLn err
      exitFailure
    Right _ -> return ()

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