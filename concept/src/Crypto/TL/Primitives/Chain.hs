{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Crypto.TL.Primitives.Chain
  ( ChainHead(..)
  , createChain, solveChain, solveChain'
  , numTowersInChain, numHashesInChain
  , resumeChainFrom, resumeChainFromMap
  ) where

import Control.Monad (foldM, replicateM, unless)
import Control.Monad.Except (MonadError(..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.State.Strict (StateT)
import qualified Control.Monad.Trans.State.Strict as S 
import Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Serialize (Putter, Get, Serialize(..), getWord64le, putWord64le)

import Crypto.TL.Primitives.BulkHashFunc
import Crypto.TL.Primitives.Checksum
import Crypto.TL.Primitives.EncryptedHash
import Crypto.TL.Primitives.Hash
import Crypto.TL.Primitives.HashFunc
import Crypto.TL.Primitives.Tower

-- Start at ChainHead at Hash, hash Int times, working up to Checksum (and verify)
-- then on to Chain links, if Chain is Empty, then verified Hash is the stop point
-- otherwise on to the other links!
data ChainHead = ChainHead !Int !Hash !Checksum !Chain
  deriving Eq

instance Show ChainHead where
  show (ChainHead height hash checksum chain) = 
    "ChainHead: " ++ "\n" ++
    tab ++ "Height: " ++ show height ++ "\n" ++
    tab ++ "Start Hash: " ++ show hash ++ "\n" ++
    tab ++ "Checksum: " ++ show checksum ++ "\n" ++
    show chain
    where tab :: String
          tab = "  "

-- Need to decrypt EncryptedHash with Hash from previous checksummed Hash
-- and then hash up to Checksum (and verify), then on to next link
data Chain = Chain !EncryptedHash !Checksum !Chain | Empty
  deriving Eq

instance Show Chain where
  show (Chain ehash checksum chain) = 
    "ChainLink: " ++ "\n" ++
    tab ++ "Encrypted Hash: " ++ show ehash ++ "\n" ++
    tab ++ "Checksum: " ++ show checksum ++ "\n" ++
    show chain
    where tab :: String
          tab = "  "
  show Empty = ""

instance Serialize ChainHead where
  put (ChainHead towerSize hash checksum chain) = 
    do
      let numLinks = chainNumLinks chain
      putWord64le $ fromIntegral numLinks
      putWord64le $ fromIntegral towerSize
      put hash
      put checksum
      putChain numLinks chain

  get = 
    do
      numLinks <- getChainNumLinks
      size <- fromIntegral <$> getWord64le
      hash <- get
      checksum <- get
      chain <- getChain numLinks
      return $ ChainHead size hash checksum chain

putChain :: Int -> Putter Chain
putChain 0 Empty = pure ()
putChain i (Chain ehash checksum chain) =
  do
    put ehash
    put checksum
    let i' = i-1
    i' `seq` putChain i' chain
putChain _ Empty = error "Invalid Size of chain! This should not happen as we count links just before"

getChain :: Int -> Get Chain
getChain 0 = pure Empty
getChain i = 
  do
    ehash <- get
    checksum <- get
    let i' = i-1
    chain <- i' `seq` getChain i'
    pure $ Chain ehash checksum chain
    
chainNumLinks :: Chain -> Int
chainNumLinks = numLinks' 0
  where
    numLinks' :: Int -> Chain -> Int
    numLinks' i Empty = i
    numLinks' i (Chain _ _ c) = i' `seq` numLinks' i' c
      where i' = i+1

getChainNumLinks :: Get Int
getChainNumLinks = fromIntegral <$> getWord64le

resumeChainFromMap :: Map Checksum Hash -> ChainHead -> IO (Int, Either Hash ChainHead)
resumeChainFromMap hashMap chainHead@(ChainHead hashesPerTower _ startChecksum chain) =
  S.execStateT (resumeChainFromHashes' hashesPerTower hashMap startChecksum chain) (0, Right chainHead)

resumeChainFromHashes' :: Int -> Map Checksum Hash -> Checksum -> Chain -> StateT (Int, Either Hash ChainHead) IO ()
resumeChainFromHashes' hashesPerTower hashMap = go 0 
  where 
    go :: Int -> Checksum -> Chain -> StateT (Int, Either Hash ChainHead) IO ()
    go !i !checksum !Empty =
      case M.lookup checksum hashMap of
        Just hash -> S.put (i, Left hash)
        Nothing -> return ()
    go !i !checksum (Chain !encryptedHash !checksum' !chain') = do
      case M.lookup checksum hashMap of
        Just !hash -> 
          let !chainHead = ChainHead hashesPerTower (decryptHash hash encryptedHash) checksum' chain'
          in chainHead `seq` S.put (i, Right chainHead)
        Nothing -> return ()
      let i' = i + 1
      i' `seq` go i' checksum' chain'

resumeChainFrom :: HashFunc -> Hash -> ChainHead -> IO (Maybe (Int, Either Hash ChainHead))
resumeChainFrom hashFunc hash (ChainHead hashesPerTower _ startChecksum chain) = do
  hashChecksum <- calcChecksum hashFunc hash
  return $ resumeChainFrom' hashesPerTower hash hashChecksum startChecksum chain

resumeChainFrom' :: Int -> Hash -> Checksum -> Checksum -> Chain -> Maybe (Int, Either Hash ChainHead)
resumeChainFrom' hashesPerTower hash hashChecksum = go 0
  where 
    go i thisChecksum Empty
      | hashChecksum == thisChecksum = Just (i, Left hash)
      | otherwise = Nothing
    go i thisChecksum (Chain encryptedHash chainChecksum chain')
      | hashChecksum == thisChecksum = Just (i, Right $ ChainHead hashesPerTower (decryptHash hash encryptedHash) chainChecksum chain')
      | otherwise = i' `seq` go i' chainChecksum chain'
      where i' = i+1

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
              let dMsg = decryptHash h' e' 
              dMsg `seq` solveChain'' $ ChainHead towerSize dMsg c' chain'

numTowersInChain :: ChainHead -> Int
numTowersInChain (ChainHead _ _ _ chain) = succ $ chainNumLinks chain

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
  let eHash = encryptHash (towerEnd t) h
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