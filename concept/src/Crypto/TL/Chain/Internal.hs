module Crypto.TL.Chain.Internal 
  ( Tower(..), ChainHead(..), Chain(..)
  ) where

import Crypto.TL.Primitives (Hash, EncryptedHash, Checksum)

import Data.Serialize (Putter, Get, Serialize(..), getWord64le, putWord64le)

data Tower = 
  Tower
  { towerSize :: !Int
  , towerStart :: !Hash
  , towerEnd :: !Hash
  } deriving Show

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

instance Serialize ChainHead where
  put (ChainHead len hash checksum chain) = 
    do
      let numLinks = chainNumLinks chain
      putWord64le $ fromIntegral numLinks
      putWord64le $ fromIntegral len
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
putChain i (Chain len ehash checksum chain) =
  do
    putWord64le $ fromIntegral len
    put ehash
    put checksum
    let i' = i-1
    i' `seq` putChain i' chain
putChain _ Empty = error "Invalid Size of chain! This should not happen as we count links just before"

getChain :: Int -> Get Chain
getChain 0 = pure Empty
getChain i = 
  do
    len <- fromIntegral <$> getWord64le
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

getChainNumLinks :: Get Int
getChainNumLinks = fromIntegral <$> getWord64le