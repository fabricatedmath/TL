{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module TL.Solve where

import Control.Monad.Except (ExceptT, MonadError(..), runExceptT)
import Control.Monad.IO.Class
import Control.Monad.State
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Options.Applicative
import System.Exit (exitFailure, exitSuccess)

import Crypto.TL
import Crypto.TL.Archive
import Crypto.TL.Util.HashUnits
import Crypto.TL.Util.HashesFromFile

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

skippingHashesChecksumMap
  :: MonadIO m
  => HashFunc
  -> Maybe Hash
  -> Maybe FilePath
  -> ExceptT String m (Map Checksum Hash)
skippingHashesChecksumMap hashFunc mskippingHash mresumeFile = do
  !checksumMapForHash <- getChecksumMapForHash mskippingHash
  !checksumMapForFile <- getChecksumMapForFile mresumeFile
  let !checksumMap = Map.union checksumMapForHash checksumMapForFile
  return checksumMap
    where getChecksumMapForHash Nothing = return Map.empty
          getChecksumMapForHash (Just hash) = do
            checksum <- liftIO $ calcChecksum hashFunc hash
            return $ Map.singleton checksum hash

          getChecksumMapForFile Nothing = return Map.empty
          getChecksumMapForFile (Just resumeFile) = do
            liftIO $ putStrLn $ "Getting hashes from file: " <> show resumeFile
            !checksumMap <- getFilesChecksumMap hashFunc resumeFile
            liftIO $ putStrLn $ "Found " <> show (Map.size checksumMap) <> " potential hashes in file" <> "\n"
            return checksumMap

trySkippingHashes 
  :: MonadIO m
  => HashFunc 
  -> Maybe Hash 
  -> Maybe FilePath 
  -> ChainHead 
  -> FilePath -- in file
  -> FilePath -- out file
  -> ExceptT String m (Int, ChainHead)
trySkippingHashes _ Nothing Nothing chainHead _ _ = return (0, chainHead)
trySkippingHashes hashFunc mskippingHash mresumeFile chainHead inFile outFile = do
  !checksumMap <- skippingHashesChecksumMap hashFunc mskippingHash mresumeFile
  mskippedChain <- liftIO $ resumeChainFromMap checksumMap chainHead
  case mskippedChain of
    (0, _) -> throwError "Failed to find any supplied hashes in chain, exiting.."
    (_skippedTowers, Left solutionHash) -> liftIO $ do
      putStrLn $ "A supplied hash was found to be the solving hash for the chain! Decrypting.." <> "\n"
      putStrLn $ "Solution hash is: " <> show solutionHash <> "\n"
      decryptTLA inFile outFile solutionHash
      putStrLn $ "Wrote decrypted file to " <> outFile
      exitSuccess
    (skippedTowers, Right skippedChain) -> liftIO $ do
      putStrLn $ "A supplied hash was found in the chain! Skipping " <> show skippedTowers <> " towers.." <> "\n"
      return (skippedTowers, skippedChain)

solve :: Solve -> IO ()
solve (Solve silent mresume mresumeFile inFile) = do
  tlaHeader <- readTLAHeader inFile
  Just (name, hashFunc) <- getBestHashFunc
  putStrLn $ "Using " <> name <> " Hash Function" <> "\n"
  let !chain = tlaGetChainHead tlaHeader
      numTowers = numTowersInChain chain
      numHashes = numHashesInChain chain
  unless silent $ putStrLn $ "Solving chain with " <> show numTowers <> " towers and " <> stringifyHash numHashes <> "\n"
  let outFile = tlaGetFileName tlaHeader
  result <- runExceptT $ do
    (startingTower, chain') <- trySkippingHashes hashFunc mresume mresumeFile chain inFile outFile
    flip evalStateT startingTower $ do
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