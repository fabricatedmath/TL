module Crypto.TL.Chain 
  ( createChain, solveChain
  , ChainHead, Tower(..), foldTowers
  ) where

import Crypto.TL.Primitives 

import Control.Monad (replicateM)

import Data.Foldable (foldl')

import Data.List.NonEmpty (NonEmpty(..), nonEmpty)

import Data.Serialize (Putter, Get, Serialize(..), getInt64le, putInt64le)

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
data Chain = Chain !Int !EncryptedHash !Checksum !Chain | Empty
  deriving Eq

instance Show Chain where
  show (Chain height ehash checksum chain) = 
    "ChainLink: " ++ "\n" ++
    tab ++ "Height: " ++ show height ++ "\n" ++
    tab ++ "Encrypted Hash: " ++ show ehash ++ "\n" ++
    tab ++ "Checksum: " ++ show checksum ++ "\n" ++
    show chain
    where tab :: String
          tab = "  "
  show Empty = ""

putChain :: Int -> Putter Chain
putChain 0 Empty = pure ()
putChain i (Chain len ehash checksum chain) =
  do
    putInt64le $ fromIntegral len
    put ehash
    put checksum
    let i' = i-1
    i' `seq` putChain i' chain
putChain _ Empty = error "Invalid Size of chain! This should not happen as we count links just before"

getChain :: Int -> Get Chain
getChain 0 = pure Empty
getChain i = 
  do
    len <- fromIntegral <$> getInt64le
    ehash <- get
    checksum <- get
    let i' = i-1
    chain <- i' `seq` getChain i'
    pure $ Chain len ehash checksum chain

chainNumLinks :: Chain -> Int
chainNumLinks = numLinks' 0
  where
    numLinks' :: Int -> Chain -> Int
    numLinks' i Empty = i
    numLinks' i (Chain _ _ _ c) = i' `seq` numLinks' i' c
      where i' = i+1

instance Serialize ChainHead where
  put (ChainHead len hash checksum chain) = 
    do
      putInt64le $ fromIntegral len
      put hash
      put checksum
      let numLinks = chainNumLinks chain
      putInt64le $ fromIntegral numLinks
      putChain numLinks chain

  get = 
    do
      size <- fromIntegral <$> getInt64le
      hash <- get
      checksum <- get
      numLinks <- fromIntegral <$> getInt64le
      chain <- getChain numLinks
      return $ ChainHead size hash checksum chain

solveChain :: Hashable a => HashMode a -> ChainHead -> Either String Hash
solveChain mode (ChainHead i h c chain) =
  do
    let h' = hashIter mode i h
    case verifyChecksum mode c h' of
      False -> Left "Failed to match!"
      True ->
        case chain of
          Empty -> pure h'
          Chain i' e' c' chain' -> 
            do
              let dMsg = decrypt h' e' 
              solveChain mode $ ChainHead i' dMsg c' chain'

createChain :: Hashable a => HashMode a -> Int -> Int -> IO (Maybe (Hash, ChainHead))
createChain mode n i = 
  do
    mtowers <- buildTowers mode n i
    return $ do
      towers <- mtowers
      return $ towers `seq` foldTowers mode towers

data Tower = 
  Tower
  { towerSize :: !Int
  , towerStart :: !Hash
  , towerEnd :: !Hash
  } deriving Show

foldTowers :: Hashable a => HashMode a -> NonEmpty Tower -> (Hash, ChainHead)
foldTowers mode (t :| ts) = 
  (towerEnd t, chain)
    where chainHead = ChainHead (towerSize t) (towerStart t) (calcChecksum mode $ towerEnd t)  Empty
          chain = foldl' (foldTower mode) chainHead ts 

foldTower :: Hashable a => HashMode a -> ChainHead -> Tower -> ChainHead
foldTower mode (ChainHead i h c chain) t = 
  ChainHead (towerSize t) (towerStart t) (calcChecksum mode $ towerEnd t)  chain'
    where eHash = encrypt (towerEnd t) h
          chain' = Chain i eHash c chain

buildTowers :: Hashable a => HashMode a -> Int -> Int -> IO (Maybe (NonEmpty Tower))
buildTowers mode n i = 
  do
    towers <- replicateM n $ (\h -> Tower i h $ hashIter mode i h) <$> randomHash
    return $ nonEmpty towers