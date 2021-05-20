{-# LANGUAGE FlexibleContexts #-}

module Crypto.TL.Chain 
  ( createChain, solveChain, solveChain'
  , ChainHead, Tower(..), foldTowers
  , getNumChainBytes, numTowersInChain, numHashesInChain
  ) where

import Control.Monad (foldM, replicateM, unless)
import Control.Monad.Except (MonadError(..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.List.NonEmpty (NonEmpty(..), nonEmpty)

import Crypto.TL.Chain.Internal (Tower(..), ChainHead(..), Chain(..), getNumChainBytes)
import Crypto.TL.Primitives
import Crypto.TL.Types

solveChain :: (MonadError String m, MonadIO m) => HashFunc -> ChainHead -> m Hash
solveChain = solveChain' noreport noreport
    where noreport = const $ return ()

solveChain' 
  :: (MonadError String m, MonadIO m) 
  => (Int -> m ()) -- report starting on a tower of length Int
  -> (Hash -> m ()) -- report tower solved and verified
  -> HashFunc
  -> ChainHead
  -> m Hash
solveChain' startReporter solveReporter hashFunc = solveChain'' 
  where 
    solveChain'' (ChainHead i h c chain) = 
      do
        startReporter i
        h' <- liftIO $ hashFunc i h
        isVerified <- liftIO $ verifyChecksum hashFunc c h'
        unless isVerified $ throwError "Failed to match hash!"
        solveReporter h'
        case chain of
          Empty -> return h'
          Chain i' e' c' chain' -> 
            do
              let dMsg = decrypt h' e' 
              dMsg `seq` solveChain'' $ ChainHead i' dMsg c' chain'

createChain :: HashFunc -> Int -> Int -> IO (Maybe (Hash, ChainHead))
createChain hashFunc n i = do
  mtowers <- buildTowers hashFunc n i
  maybe (return Nothing) (fmap Just . foldTowers hashFunc) mtowers

numTowersInChain :: ChainHead -> Int
numTowersInChain (ChainHead _ _ _ chain) = go 1 chain
  where 
    go i Empty = i
    go i (Chain _ _ _ c) = i' `seq` go i' c
      where i' = i+1

numHashesInChain :: ChainHead -> Int
numHashesInChain (ChainHead n' _ _ chain) = go n' chain
  where 
    go i Empty = i
    go i (Chain n _ _ c) = i' `seq` go i' c
      where i' = i+n

foldTowers :: HashFunc -> NonEmpty Tower -> IO (Hash, ChainHead)
foldTowers hashFunc (t :| ts) = do
  checksum <- calcChecksum hashFunc $ towerEnd t
  let chainHead = ChainHead (towerSize t) (towerStart t) checksum Empty
  chain <- foldM (foldTower hashFunc) chainHead ts 
  return (towerEnd t, chain)

foldTower :: HashFunc -> ChainHead -> Tower -> IO ChainHead
foldTower hashFunc (ChainHead i h c chain) t = do
  checksum <- calcChecksum hashFunc $ towerEnd t
  let eHash = encrypt (towerEnd t) h
      chain' = Chain i eHash c chain
  return $ ChainHead (towerSize t) (towerStart t) checksum chain'

buildTowers :: HashFunc -> Int -> Int -> IO (Maybe (NonEmpty Tower))
buildTowers hashFunc n i = 
  do
    towers <- replicateM n $ randomHash >>= (\h -> Tower i h <$> hashFunc i h) 
    return $ nonEmpty towers