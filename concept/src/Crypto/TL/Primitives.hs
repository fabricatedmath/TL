{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Crypto.TL.Primitives 
  ( Hash, Checksum, EncryptedHash
  , sha256, sha256iter, sha256iterFast
  , sha256checksum, verifyChecksum
  , randomHash
  , encrypt, decrypt
  ) where

import Control.Monad (replicateM, when)

import qualified Crypto.Hash as Hash (hashWith, SHA256(..))

import qualified Crypto.Random.Types as CRT (getRandomBytes)

import Data.Bits (xor)

import qualified Data.ByteArray as ByteArray

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (copy, map, pack, packZipWith, unpack)
import Data.ByteString.Base16 (encodeBase16')

import Data.ByteString.Unsafe (unsafeUseAsCString)

import Data.Serialize

import Foreign.C.String (CString)

import System.IO.Unsafe (unsafePerformIO)

newtype Hash = 
    Hash 
    { unHash :: ByteString
    } deriving Eq
  
instance Serialize Hash where
  put (Hash bs) = 
    do
      let unpackedBS = BS.unpack bs
      when (length unpackedBS /= 32) $ error "Hash has bad length!"
      mapM_ putWord8 unpackedBS
  get = Hash . BS.pack <$> replicateM 32 getWord8

newtype Checksum = 
    Checksum 
    { unChecksum :: Hash 
    } deriving (Eq, Serialize, Show)

newtype EncryptedHash = 
    EncryptedHash 
    { unEncryptedHash :: Hash
    } deriving (Eq, Serialize, Show)

instance Show Hash where
    show = show . encodeBase16' . unHash

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

randomHash :: IO Hash
randomHash = Hash <$> CRT.getRandomBytes 32

-- Add 1 to each byte (with overflow) and then hash to yield our checkpoint
sha256checksum :: Hash -> Checksum
sha256checksum = Checksum . sha256' . Hash . BS.map (+1) . unHash

verifyChecksum :: Checksum -> Hash -> Bool
verifyChecksum checksum hash = checksum == sha256checksum hash

-- To Encrypt, we xor the ending of tower n and the start of tower (n+1)
encrypt :: Hash -> Hash -> EncryptedHash
encrypt (Hash bs1) (Hash bs2) = EncryptedHash $ Hash $ BS.packZipWith xor bs1 bs2

-- To Decrypt, we xor the ending of tower n and the result of the encrypt of tower (n+1)
decrypt :: Hash -> EncryptedHash -> Hash
decrypt hashKey (EncryptedHash eHash) = unEncryptedHash $ encrypt hashKey eHash

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