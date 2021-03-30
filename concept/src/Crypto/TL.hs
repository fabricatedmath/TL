module Crypto.TL 
  ( solveChain
  , createChain
  ) where

import Crypto.Hash
import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types (BlockCipher(..), Cipher(..))
import qualified Crypto.Error as CE (CryptoError(..), eitherCryptoError)

import qualified Crypto.Random.Types as CRT

import qualified Data.ByteArray as ByteArray

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Builder as Builder

import Data.ByteString.Lazy (toStrict)

import Data.Foldable (foldlM)

import Data.List.NonEmpty (NonEmpty(..))

import Data.Word (Word64)

newtype Hash = Hash { unHash :: ByteString }
  deriving Eq

instance Show Hash where
  show (Hash bs) = show $ Base16.encode bs

newtype Checkpoint = Checkpoint Hash
  deriving (Eq, Show)

newtype EncryptedHash = EncryptedHash ByteString

instance Show EncryptedHash where
  show (EncryptedHash bs) = show $ Base16.encode bs

data Tower = 
  Tower
  { towerSize :: Int
  , towerStart :: Hash
  , towerEnd :: Hash 
  } deriving Show

data Chain = Chain Int Checkpoint EncryptedHash Chain | Empty
  deriving Show

data ChainHead = ChainHead Int Checkpoint Hash Chain
  deriving Show

data CryptoTLError = MatchingIssue Checkpoint Hash | CryptoError CE.CryptoError
  deriving Show

-- Module Export

solveChain :: ChainHead -> Either CryptoTLError Hash
solveChain (ChainHead i c h chain) =
  do
    let h' = iterate' i sha256iter h
    case verifyCheckpoint c h' of
      False -> Left $ MatchingIssue c h'
      True -> 
        case chain of
          Empty -> pure h'
          Chain i' c' e' chain' -> 
            do
              dMsg <- wrapCE $ decryptHash h' e' 
              solveChain $ ChainHead i' c' dMsg chain'

  where wrapCE :: Either CE.CryptoError a -> Either CryptoTLError a
        wrapCE = either (Left . CryptoError) pure

createChain :: Int -> Int -> IO (Either CE.CryptoError (Hash, ChainHead))
createChain n i = foldTowers <$> randomHashTowers n i

-- Higher Level Helpers

foldTowers :: NonEmpty Tower -> Either CE.CryptoError (Hash, ChainHead)
foldTowers (t :| ts) = 
  do
    let chainHead = ChainHead (towerSize t) (sha256checkpoint $ towerEnd t) (towerStart t) Empty
    chain <- foldlM foldTower chainHead ts 
    pure $ (towerEnd t, chain)
  where
    foldTower :: ChainHead -> Tower -> Either CE.CryptoError ChainHead
    foldTower (ChainHead i c h chain) t = 
      do
        eHash <- encryptHash (towerEnd t) h
        let chain' = Chain i c eHash chain
        return $ ChainHead (towerSize t) (sha256checkpoint $ towerEnd t) (towerStart t) chain'

randomHashTowers :: Int -> Int -> IO (NonEmpty Tower)
randomHashTowers n i = sequence $ nonEmptyReplicate n (randomHashTower i)

randomHashTower :: Int -> IO Tower
randomHashTower i = hashTower i <$> randomHash
  where
    hashTower :: Int -> Hash -> Tower
    hashTower i h = 
      Tower 
      { towerSize = i
      , towerStart = h
      , towerEnd = iterate' i sha256iter h
      }

    randomHash :: IO Hash
    randomHash = Hash <$> CRT.getRandomBytes 32

-- Crypto Stuff

sha256 :: ByteString -> Hash
sha256 = Hash . ByteArray.convert . hashWith SHA256

sha256iter :: Hash -> Hash
sha256iter = sha256 . unHash

sha256checkpoint :: Hash -> Checkpoint
sha256checkpoint = Checkpoint . sha256 . BS.map (+1) . unHash

verifyCheckpoint :: Checkpoint -> Hash -> Bool
verifyCheckpoint checkpoint hash = checkpoint == sha256checkpoint hash

encryptHash :: Hash -> Hash -> Either CE.CryptoError EncryptedHash
encryptHash hashKey hashMsg = 
  fmap (\c -> EncryptedHash $ ecbEncrypt c $ unHash hashMsg) i
    where i :: Either CE.CryptoError AES256
          i = CE.eitherCryptoError $ cipherInit $ unHash hashKey

decryptHash :: Hash -> EncryptedHash -> Either CE.CryptoError Hash
decryptHash hashKey (EncryptedHash msg) = 
  fmap (\c -> Hash $ ecbDecrypt c msg) i
    where i:: Either CE.CryptoError AES256
          i = CE.eitherCryptoError $ cipherInit $ unHash hashKey

-- Helpers

nonEmptySingleton :: a -> NonEmpty a
nonEmptySingleton a = a :| []

nonEmptyReplicate :: Int -> a -> NonEmpty a
nonEmptyReplicate i a = a :| replicate (pred i) a

iterate' :: Int -> (a -> a) -> a -> a
iterate' n f a = iterate'' n a
  where 
    iterate'' n a
      | n <= 0 = a
      | otherwise = a' `seq` n' `seq` iterate' n' f a'
        where
          a' = f a 
          n' = n-1
