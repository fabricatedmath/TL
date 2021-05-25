{-# LANGUAGE FlexibleContexts #-}

module Crypto.TL.Chain 
  ( solveChain, solveChain'
  , ChainHead, Tower(..), foldTowers
  , numTowersInChain, numHashesInChain
  , createChain
  ) where

import Control.Monad (foldM, unless)
import Control.Monad.Except (MonadError(..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.List.NonEmpty (NonEmpty(..))

import Control.Monad (replicateM)

import Data.List.NonEmpty (nonEmpty)

import Crypto.TL.Chain.Internal (ChainHead(..), Chain(..))
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
    solveChain'' (ChainHead towerSize h c chain) = 
      do
        startReporter towerSize
        h' <- liftIO $ hashFunc towerSize h
        isVerified <- liftIO $ verifyChecksum hashFunc c h'
        unless isVerified $ throwError "Failed to match hash!"
        solveReporter h'
        case chain of
          Empty -> return h'
          Chain e' c' chain' -> 
            do
              let dMsg = decrypt h' e' 
              dMsg `seq` solveChain'' $ ChainHead towerSize dMsg c' chain'

numTowersInChain :: ChainHead -> Int
numTowersInChain (ChainHead _ _ _ chain) = go 1 chain
  where 
    go i Empty = i
    go i (Chain _ _ c) = i' `seq` go i' c
      where i' = i+1

numHashesInChain :: ChainHead -> Int
numHashesInChain chainHead@(ChainHead towerSize _ _ _) = numTowersInChain chainHead * towerSize

foldTowers :: HashFunc -> Int -> NonEmpty Tower -> IO (Hash, ChainHead)
foldTowers hashFunc numIters (t :| ts) = do
  checksum <- calcChecksum hashFunc $ towerEnd t
  let chainHead = ChainHead numIters (towerStart t) checksum Empty
  chain <- foldM (foldTower hashFunc) chainHead ts 
  return (towerEnd t, chain)

foldTower :: HashFunc -> ChainHead -> Tower -> IO ChainHead
foldTower hashFunc (ChainHead i h c chain) t = do
  checksum <- calcChecksum hashFunc $ towerEnd t
  let eHash = encrypt (towerEnd t) h
      chain' = Chain eHash c chain
  return $ ChainHead i (towerStart t) checksum chain'

createChain 
  :: HashFunc
  -> BulkHashFunc
  -> Int -- num towers
  -> Int -- num iters per tower
  -> IO (Maybe (Hash, ChainHead))
createChain hashFunc bulkHashFunc numTowers numIters = do
  mtowers <- nonEmpty <$> (replicateM numTowers randomHash >>= bulkHashFunc numIters)
  maybe (return Nothing) (fmap Just . foldTowers hashFunc numIters) mtowers