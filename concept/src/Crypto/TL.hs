-- {-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Crypto.TL 
  ( createChain, solveChain
  , sha256, sha256iter, sha256iterFast
  ) where

import qualified Crypto.Hash as Hash (hashWith, SHA256(..))

import qualified Crypto.Random.Types as CRT (getRandomBytes)

import Data.Bits (xor)

import qualified Data.ByteArray as ByteArray

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (copy, map, packZipWith)
import Data.ByteString.Base16 (encodeBase16')

import Data.ByteString.Unsafe (unsafeUseAsCString)

import Data.Foldable (foldl')

import Data.List.NonEmpty (NonEmpty(..))

-- import Data.Serialize

import Foreign.C.String (CString)

import System.IO.Unsafe (unsafePerformIO)

newtype Hash = 
    Hash 
    { unHash :: ByteString
    } deriving Eq

newtype Checksum = 
    Checksum 
    { unChecksum :: Hash 
    } deriving (Eq, Show)

newtype EncryptedHash = 
    EncryptedHash 
    { unEncryptedHash :: Hash
    } deriving (Eq, Show)

instance Show Hash where
    show = show . encodeBase16' . unHash

data Tower = 
  Tower
  { towerSize :: Int
  , towerStart :: Hash
  , towerEnd :: Hash 
  } deriving Show

-- Start at ChainHead at Hash, hash Int times, working up to Checksum (and verify)
-- then on to Chain links, if Chain is Empty, then verified Hash is the stop point
-- otherwise, on to the other links!
data ChainHead = ChainHead !Int !Hash !Checksum !Chain
  deriving (Eq, Show)

-- Need to decrypt EncryptedHash with Hash from previous checksummed Hash
-- and then hash up to Checksum (and verify), then on to next link
data Chain = Chain !Int !EncryptedHash !Checksum !Chain | Empty
  deriving (Eq, Show)

{-
putChain :: Int -> Putter Chain
putChain 0 Empty = pure ()
putChain i (Chain len ehash checksum chain) =
  do
    putInt64be $ fromIntegral len
    put ehash
    put checksum
    let i' = i-1
    i' `seq` putChain i' chain
putChain _ Empty = error "Invalid Size of chain! This should not happen as we count links just before"

getChain :: Int -> Get Chain
getChain 0 = pure Empty
getChain i = 
  do
    len <- fromIntegral <$> getInt64be
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
      putInt64be $ fromIntegral len
      put hash
      put checksum
      let numLinks = chainNumLinks chain
      putInt64be $ fromIntegral numLinks
      putChain numLinks chain

  get = 
    do
      size <- fromIntegral <$> getInt64be
      hash <- get
      checksum <- get
      numLinks <- fromIntegral <$> getInt64be
      chain <- getChain numLinks
      return $ ChainHead size hash checksum chain
-}
-- Module Export

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

-- Higher Level Helpers

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
randomHashTowers n i = sequence $ nonEmptyReplicate n (randomHashTower i)

randomHashTower :: Int -> IO Tower
randomHashTower i = 
  do
    h <- randomHash
    pure $ Tower { towerSize = i, towerStart = h, towerEnd = sha256iter i h}

randomHash :: IO Hash
randomHash = Hash <$> CRT.getRandomBytes 32

-- Crypto Stuff

sha256 :: ByteString -> Hash
sha256 = Hash . ByteArray.convert . Hash.hashWith Hash.SHA256

sha256' :: Hash -> Hash
sha256' = Hash . ByteArray.convert . Hash.hashWith Hash.SHA256 . unHash

sha256iter :: Int -> Hash -> Hash
sha256iter num = iterate' num sha256'
    where
        iterate' :: Int -> (a -> a) -> a -> a
        iterate' n f ainit = iterate'' n ainit
            where 
                iterate'' i a
                    | i <= 0 = a
                    | otherwise = a' `seq` i' `seq` iterate' i' f a'
                        where
                        a' = f a 
                        i' = i-1

-- Add 1 to each byte (with overflow) and then hash to yield our checkpoint
sha256checksum :: Hash -> Checksum
sha256checksum = Checksum . sha256' . Hash . BS.map (+1) . unHash

verifyChecksum :: Checksum -> Hash -> Bool
verifyChecksum checksum hash = checksum == sha256checksum hash

encrypt :: Hash -> Hash -> EncryptedHash
encrypt (Hash bs1) (Hash bs2) = EncryptedHash $ Hash $ BS.packZipWith xor bs1 bs2

decrypt :: Hash -> EncryptedHash -> Hash
decrypt hashKey (EncryptedHash eHash) = unEncryptedHash $ encrypt hashKey eHash

-- Helpers

nonEmptyReplicate :: Int -> a -> NonEmpty a
nonEmptyReplicate i a = a :| replicate (pred i) a

sha256iterFast :: Int -> Hash -> Hash
sha256iterFast i hash = unsafePerformIO $ sha256iterFast'
  where
    sha256iterFast' :: IO Hash
    sha256iterFast' =
        do
            let bs' = BS.copy $ unHash hash
            unsafeUseAsCString bs' (c_sha256_iter i)
            pure $ Hash bs'

foreign import ccall safe "sha256_iter"
  c_sha256_iter :: Int -> CString-> IO ()