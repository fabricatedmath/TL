{-# LANGUAGE FlexibleContexts #-}

module Crypto.TL.Chain 
  ( createChain, solveChain, solveChain'
  , ChainHead, Tower(..), foldTowers
  , getNumChainBytes, numTowersInChain, numHashesInChain
  ) where

import Control.Monad (replicateM, unless)
import Control.Monad.Except (MonadError(..))
import Data.Foldable (foldl')
import Data.List.NonEmpty (NonEmpty(..), nonEmpty)

import Crypto.TL.Chain.Internal (Tower(..), ChainHead(..), Chain(..), getNumChainBytes)
import Crypto.TL.Primitives
import Crypto.TL.Types

solveChain :: (MonadError String m) => HashFunc -> ChainHead -> m Hash
solveChain = solveChain' noreport noreport
    where noreport = const $ return ()

solveChain' 
  :: (MonadError String m) 
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
        let h' = hashFunc i h
        unless (verifyChecksum hashFunc c h') $ throwError "Failed to match hash!"
        solveReporter h'
        case chain of
          Empty -> return h'
          Chain i' e' c' chain' -> 
            do
              let dMsg = decrypt h' e' 
              dMsg `seq` solveChain'' $ ChainHead i' dMsg c' chain'

createChain :: HashFunc -> Int -> Int -> IO (Maybe (Hash, ChainHead))
createChain hashFunc n i = 
  do
    mtowers <- buildTowers hashFunc n i
    return $ do
      towers <- mtowers
      towers `seq` return $ foldTowers hashFunc towers

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

foldTowers :: HashFunc -> NonEmpty Tower -> (Hash, ChainHead)
foldTowers hashFunc (t :| ts) = (towerEnd t, chain)
  where chainHead = ChainHead (towerSize t) (towerStart t) (calcChecksum hashFunc $ towerEnd t)  Empty
        chain = foldl' (foldTower hashFunc) chainHead ts 

foldTower :: HashFunc -> ChainHead -> Tower -> ChainHead
foldTower hashFunc (ChainHead i h c chain) t = 
  ChainHead (towerSize t) (towerStart t) (calcChecksum hashFunc $ towerEnd t) chain'
    where eHash = encrypt (towerEnd t) h
          chain' = Chain i eHash c chain

buildTowers :: HashFunc -> Int -> Int -> IO (Maybe (NonEmpty Tower))
buildTowers hashFunc n i = 
  do
    towers <- replicateM n $ (\h -> Tower i h $ hashFunc i h) <$> randomHash
    return $ nonEmpty towers