{-# LANGUAGE FlexibleContexts #-}

module Crypto.TL.Chain 
  ( createChain, solveChain, solveChain'
  , ChainHead, Tower(..), foldTowers
  , getNumChainBytes
  ) where

import Crypto.TL.Chain.Internal (Tower(..), ChainHead(..), Chain(..), getNumChainBytes)
import Crypto.TL.Primitives 

import Control.Monad (replicateM, unless)
import Control.Monad.Except (MonadError(..), runExcept)

import Data.Foldable (foldl')

import Data.List.NonEmpty (NonEmpty(..), nonEmpty)

solveChain :: Hashable a => HashMode a -> ChainHead -> Either String Hash
solveChain mode chain = runExcept $ solveChain' noreport noreport mode chain
    where noreport = const $ return ()

solveChain' 
  :: (Hashable a, Monad m, MonadError String m) 
  => (Int -> m ()) -- report starting on a tower of length Int
  -> (Hash -> m ()) -- report tower solved and verified
  -> HashMode a 
  -> ChainHead 
  -> m Hash
solveChain' startReporter solveReporter mode = solveChain'' 
    where 
      solveChain'' (ChainHead i h c chain) = 
        do
          startReporter i
          let h' = hashIter mode i h
          unless (verifyChecksum mode c h') $ throwError "Failed to match hash!"
          solveReporter h'
          case chain of
            Empty -> return h'
            Chain i' e' c' chain' -> 
              do
                let dMsg = decrypt h' e' 
                dMsg `seq` solveChain'' $ ChainHead i' dMsg c' chain'

createChain :: Hashable a => HashMode a -> Int -> Int -> IO (Maybe (Hash, ChainHead))
createChain mode n i = 
  do
    mtowers <- buildTowers mode n i
    return $ do
      towers <- mtowers
      return $ towers `seq` foldTowers mode towers

foldTowers :: Hashable a => HashMode a -> NonEmpty Tower -> (Hash, ChainHead)
foldTowers mode (t :| ts) = (towerEnd t, chain)
    where chainHead = ChainHead (towerSize t) (towerStart t) (calcChecksum mode $ towerEnd t)  Empty
          chain = foldl' (foldTower mode) chainHead ts 

foldTower :: Hashable a => HashMode a -> ChainHead -> Tower -> ChainHead
foldTower mode (ChainHead i h c chain) t = 
  ChainHead (towerSize t) (towerStart t) (calcChecksum mode $ towerEnd t) chain'
    where eHash = encrypt (towerEnd t) h
          chain' = Chain i eHash c chain

buildTowers :: Hashable a => HashMode a -> Int -> Int -> IO (Maybe (NonEmpty Tower))
buildTowers mode n i = 
  do
    towers <- replicateM n $ (\h -> Tower i h $ hashIter mode i h) <$> randomHash
    return $ nonEmpty towers