module Crypto.TL.Chain 
  ( createChain, solveChain
  ) where

import Crypto.TL.Primitives 

import Data.Foldable (foldl')

import Data.List.NonEmpty (NonEmpty(..))

--import Data.Serialize

-- Start at ChainHead at Hash, hash Int times, working up to Checksum (and verify)
-- then on to Chain links, if Chain is Empty, then verified Hash is the stop point
-- otherwise on to the other links!
data ChainHead = ChainHead !Int !Hash !Checksum !Chain
  deriving (Eq, Show)

-- Need to decrypt EncryptedHash with Hash from previous checksummed Hash
-- and then hash up to Checksum (and verify), then on to next link
data Chain = Chain !Int !EncryptedHash !Checksum !Chain | Empty
  deriving (Eq, Show)

solveChain :: ChainHead -> Either String Hash
solveChain (ChainHead i h c chain) =
  do
    let h' = sha256iter i h
    case verifyChecksum c h' of
      False -> Left "Failed to match!"
      True ->
        case chain of
          Empty -> pure h'
          Chain i' e' c' chain' -> 
            do
              let dMsg = decrypt h' e' 
              solveChain $ ChainHead i' dMsg c' chain'

createChain :: Int -> Int -> IO (Hash, ChainHead)
createChain n i = foldTowers <$> randomHashTowers n i

data Tower = 
  Tower
  { towerSize :: !Int
  , towerStart :: !Hash
  , towerEnd :: !Hash 
  } deriving Show

foldTowers :: NonEmpty Tower -> (Hash, ChainHead)
foldTowers (t :| ts) = 
  let chainHead = ChainHead (towerSize t) (towerStart t) (sha256checksum $ towerEnd t)  Empty
      chain = foldl' foldTower chainHead ts 
  in (towerEnd t, chain)

foldTower :: ChainHead -> Tower -> ChainHead
foldTower (ChainHead i h c chain) t = 
  let eHash = encrypt (towerEnd t) h
      chain' = Chain i eHash c chain
  in ChainHead (towerSize t) (towerStart t) (sha256checksum $ towerEnd t)  chain'

randomHashTowers :: Int -> Int -> IO (NonEmpty Tower)
randomHashTowers n i = sequence $ nonEmptyReplicate n $ randomHashTower i

randomHashTower :: Int -> IO Tower
randomHashTower i = 
  do
    h <- randomHash
    pure $ Tower { towerSize = i, towerStart = h, towerEnd = sha256iter i h}

nonEmptyReplicate :: Int -> a -> NonEmpty a
nonEmptyReplicate i a = a :| replicate (pred i) a