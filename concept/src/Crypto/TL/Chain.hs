module Crypto.TL.Chain 
  ( createChain, solveChain
  , ChainHead
  ) where

import Crypto.TL.Primitives 

import Data.Foldable (foldl')

import Data.List.NonEmpty (NonEmpty(..))

import Data.Serialize (Putter, Get, Serialize(..), getInt64le, putInt64le)

-- Start at ChainHead at Hash, hash Int times, working up to Checksum (and verify)
-- then on to Chain links, if Chain is Empty, then verified Hash is the stop point
-- otherwise on to the other links!
data ChainHead a = ChainHead !Int !(Hash a) !(Checksum a) !(Chain a)
  deriving (Eq, Show)

-- Need to decrypt EncryptedHash with Hash from previous checksummed Hash
-- and then hash up to Checksum (and verify), then on to next link
data Chain a = Chain !Int !(EncryptedHash a) !(Checksum a) !(Chain a) | Empty
  deriving (Eq, Show)

putChain :: Int -> Putter (Chain a)
putChain 0 Empty = pure ()
putChain i (Chain len ehash checksum chain) =
  do
    putInt64le $ fromIntegral len
    put ehash
    put checksum
    let i' = i-1
    i' `seq` putChain i' chain
putChain _ Empty = error "Invalid Size of chain! This should not happen as we count links just before"

getChain :: Int -> Get (Chain a)
getChain 0 = pure Empty
getChain i = 
  do
    len <- fromIntegral <$> getInt64le
    ehash <- get
    checksum <- get
    let i' = i-1
    chain <- i' `seq` getChain i'
    pure $ Chain len ehash checksum chain

chainNumLinks :: Chain a -> Int
chainNumLinks = numLinks' 0
  where
    numLinks' :: Int -> Chain a -> Int
    numLinks' i Empty = i
    numLinks' i (Chain _ _ _ c) = i' `seq` numLinks' i' c
      where i' = i+1

instance Serialize (ChainHead a) where
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

solveChain :: Hashable a => ChainHead a -> Either String (Hash a)
solveChain (ChainHead i h c chain) =
  do
    let h' = hashIter i h
    case verifyChecksum c h' of
      False -> Left "Failed to match!"
      True ->
        case chain of
          Empty -> pure h'
          Chain i' e' c' chain' -> 
            do
              let dMsg = decrypt h' e' 
              solveChain $ ChainHead i' dMsg c' chain'

createChain :: Hashable a => Int -> Int -> IO (Hash a, ChainHead a)
createChain n i = foldTowers <$> randomHashTowers n i

data Tower a = 
  Tower
  { towerSize :: !Int
  , towerStart :: !(Hash a)
  , towerEnd :: !(Hash a) 
  } deriving Show

foldTowers :: Hashable a => NonEmpty (Tower a) -> (Hash a, ChainHead a)
foldTowers (t :| ts) = 
  (towerEnd t, chain)
    where chainHead = ChainHead (towerSize t) (towerStart t) (calcChecksum $ towerEnd t)  Empty
          chain = foldl' foldTower chainHead ts 

foldTower :: Hashable a => ChainHead a -> Tower a -> ChainHead a
foldTower (ChainHead i h c chain) t = 
  ChainHead (towerSize t) (towerStart t) (calcChecksum $ towerEnd t)  chain'
    where eHash = encrypt (towerEnd t) h
          chain' = Chain i eHash c chain

randomHashTowers :: Hashable a => Int -> Int -> IO (NonEmpty (Tower a))
randomHashTowers n i = sequence $ nonEmptyReplicate n $ randomHashTower i

randomHashTower :: Hashable a => Int -> IO (Tower a)
randomHashTower i = (\h -> Tower i h $ hashIter i h) <$> randomHash

nonEmptyReplicate :: Int -> a -> NonEmpty a
nonEmptyReplicate i a = a :| replicate (pred i) a