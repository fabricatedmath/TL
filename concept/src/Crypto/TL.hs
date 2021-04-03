{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Crypto.TL 
  ( solveChain
  , createChain
  , sha256, sha256iter, iterate'
  , sha256iterFast
  ) where

import Basement.Types.Word256 (Word256(..))

import qualified Crypto.Hash as Hash
import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types (BlockCipher(..), Cipher(..))
import qualified Crypto.Error as CE (CryptoError(..), eitherCryptoError)

import qualified Crypto.Random.Types as CRT

import qualified Data.ByteArray as ByteArray

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Base16 (encodeBase16')

import Data.Either (fromRight)

import Data.Foldable (foldlM)

import Data.List.NonEmpty (NonEmpty(..))

import Data.Serialize

import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable

instance Serialize Word256 where
  put (Word256 w0 w1 w2 w3) =
    do
      putWord64le w0
      putWord64le w1
      putWord64le w2
      putWord64le w3
  get =
    do
      w0 <- getWord64le
      w1 <- getWord64le
      w2 <- getWord64le
      w3 <- getWord64le
      pure $ Word256 w0 w1 w2 w3

newtype BS256 = BS256 { unBS256 :: ByteString }
  deriving Eq

bs256ToWord256 :: BS256 -> Word256
bs256ToWord256 = fromRight undefined . decode . unBS256

word256ToBS256 :: Word256 -> BS256
word256ToBS256 = BS256 . encode

instance Serialize BS256 where
  put = put . bs256ToWord256
  get = fmap word256ToBS256 get

instance Show BS256 where
  show = show . encodeBase16' . unBS256

newtype Hash = Hash { unHash :: BS256 }
  deriving (Eq, Serialize, Show)

newtype Checkpoint = Checkpoint Hash
  deriving (Eq, Serialize, Show)

newtype EncryptedHash = EncryptedHash BS256
  deriving (Eq, Serialize, Show)

data Tower = 
  Tower
  { towerSize :: Int
  , towerStart :: Hash
  , towerEnd :: Hash 
  } deriving Show

-- Start at ChainHead at Hash, hash Int times, working up to Checkpoint (and verify)
-- then on to Chain links, if Chain is Empty, then verified Hash is the stop point
-- otherwise, on to the other links!
data ChainHead = ChainHead Int Hash Checkpoint Chain
  deriving (Eq, Show)

-- Need to decrypt EncryptedHash with Hash from previous checkpointed Hash
-- and then hash up to Checkpoint (and verify), then on to next link
data Chain = Chain Int EncryptedHash Checkpoint Chain | Empty
  deriving (Eq, Show)

putChain :: Int -> Putter Chain
putChain 0 Empty = pure ()
putChain i (Chain len ehash checkpoint chain) =
  do
    putInt64be $ fromIntegral len
    put ehash
    put checkpoint
    let i' = i-1
    i' `seq` putChain i' chain
putChain _ Empty = error "Invalid Size of chain! This should not happen as we count links just before"

getChain :: Int -> Get Chain
getChain 0 = pure Empty
getChain i = 
  do
    len <- fromIntegral <$> getInt64be
    ehash <- get
    checkpoint <- get
    let i' = i-1
    chain <- i' `seq` getChain i'
    pure $ Chain len ehash checkpoint chain

chainNumLinks :: Chain -> Int
chainNumLinks = numLinks' 0
  where
    numLinks' :: Int -> Chain -> Int
    numLinks' i Empty = i
    numLinks' i (Chain _ _ _ c) = i' `seq` numLinks' i' c
      where i' = i+1

instance Serialize ChainHead where
  put (ChainHead len hash checkpoint chain) = 
    do
      putInt64be $ fromIntegral len
      put hash
      put checkpoint
      let numLinks = chainNumLinks chain
      putInt64be $ fromIntegral numLinks
      putChain numLinks chain

  get = 
    do
      size <- fromIntegral <$> getInt64be
      hash <- get
      checkpoint <- get
      numLinks <- fromIntegral <$> getInt64be
      chain <- getChain numLinks
      return $ ChainHead size hash checkpoint chain

data CryptoTLError = MatchingIssue Checkpoint Hash | CryptoError CE.CryptoError
  deriving (Eq, Show)

-- Module Export

solveChain :: ChainHead -> Either CryptoTLError Hash
solveChain (ChainHead i h c chain) =
  do
    let h' = iterate' i sha256iter h
    case verifyCheckpoint c h' of
      False -> Left $ MatchingIssue c h'
      True -> 
        case chain of
          Empty -> pure h'
          Chain i' e' c' chain' -> 
            do
              dMsg <- wrapCE $ decryptHash h' e' 
              solveChain $ ChainHead i' dMsg c' chain'

  where wrapCE :: Either CE.CryptoError a -> Either CryptoTLError a
        wrapCE = either (Left . CryptoError) pure

createChain :: Int -> Int -> IO (Either CE.CryptoError (Hash, ChainHead))
createChain n i = foldTowers <$> randomHashTowers n i

-- Higher Level Helpers

foldTowers :: NonEmpty Tower -> Either CE.CryptoError (Hash, ChainHead)
foldTowers (t :| ts) = 
  do
    let chainHead = ChainHead (towerSize t) (towerStart t) (sha256checkpoint $ towerEnd t)  Empty
    chain <- foldlM foldTower chainHead ts 
    pure $ (towerEnd t, chain)

foldTower :: ChainHead -> Tower -> Either CE.CryptoError ChainHead
foldTower (ChainHead i h c chain) t = 
  do
    eHash <- encryptHash (towerEnd t) h
    let chain' = Chain i eHash c chain
    return $ ChainHead (towerSize t) (towerStart t) (sha256checkpoint $ towerEnd t)  chain'

randomHashTowers :: Int -> Int -> IO (NonEmpty Tower)
randomHashTowers n i = sequence $ nonEmptyReplicate n (randomHashTower i)

randomHashTower :: Int -> IO Tower
randomHashTower i = 
  do
    h <- Hash . BS256 <$> CRT.getRandomBytes 32
    pure $ Tower { towerSize = i, towerStart = h, towerEnd = iterate' i sha256iter h}

-- Crypto Stuff

sha256 :: ByteString -> Hash
sha256 = Hash . BS256 . ByteArray.convert . Hash.hashWith Hash.SHA256

sha256iter :: Hash -> Hash
sha256iter = sha256 . unBS256 . unHash

-- Add 1 to each byte (with overflow) and then hash to yield our checkpoint
sha256checkpoint :: Hash -> Checkpoint
sha256checkpoint = Checkpoint . sha256 . BS.map (+1) . unBS256 . unHash

verifyCheckpoint :: Checkpoint -> Hash -> Bool
verifyCheckpoint checkpoint hash = checkpoint == sha256checkpoint hash

encryptHash :: Hash -> Hash -> Either CE.CryptoError EncryptedHash
encryptHash hashKey hashMsg = 
  fmap (\c -> EncryptedHash $ BS256 $ ecbEncrypt c $ unBS256 $ unHash hashMsg) i
    where i :: Either CE.CryptoError AES256
          i = CE.eitherCryptoError $ cipherInit $ unBS256 $ unHash hashKey

decryptHash :: Hash -> EncryptedHash -> Either CE.CryptoError Hash
decryptHash hashKey (EncryptedHash msg) = 
  fmap (\c -> Hash . BS256 . ecbDecrypt c $ unBS256 msg) i
    where i :: Either CE.CryptoError AES256
          i = CE.eitherCryptoError $ cipherInit $ unBS256 $ unHash hashKey

-- Helpers

nonEmptyReplicate :: Int -> a -> NonEmpty a
nonEmptyReplicate i a = a :| replicate (pred i) a

iterate' :: Int -> (a -> a) -> a -> a
iterate' n f ainit = iterate'' n ainit
  where 
    iterate'' i a
      | i <= 0 = a
      | otherwise = a' `seq` i' `seq` iterate' i' f a'
        where
          a' = f a 
          i' = i-1

sha256iterFast :: Int -> Hash -> IO Hash
sha256iterFast i hash = 
  do
    let w256 = bs256ToWord256 $ unHash hash
    Hash . word256ToBS256 <$> with w256 (\ptr -> c_sha256_iter i ptr >> peek ptr)

foreign import ccall "sha256_iter"
  c_sha256_iter :: Int -> Ptr Word256 -> IO ()