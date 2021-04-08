{-# LANGUAGE OverloadedStrings #-} -- Remove Later
module Crypto.TL2 where

import qualified Crypto.Hash as Hash (hashWith, SHA256(..))

import qualified Crypto.Random.Types as CRT

import Control.Monad (replicateM)

import Data.Bits (xor)

import qualified Data.ByteArray as ByteArray (convert)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (copy, map, packZipWith)
import Data.ByteString.Base16 (decodeBase16Lenient, encodeBase16')
import Data.ByteString.Unsafe (unsafeUseAsCString)

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
    } deriving Show

instance Show Hash where
    show = show . encodeBase16' . unHash

randomHash :: IO Hash
randomHash = Hash <$> CRT.getRandomBytes 32

sha256 :: ByteString -> Hash
sha256 = Hash . ByteArray.convert . Hash.hashWith Hash.SHA256

sha256' :: Hash -> Hash
sha256' = Hash . ByteArray.convert . Hash.hashWith Hash.SHA256 . unHash

sha256iter :: Int -> Hash -> Hash
sha256iter num = iterate' num (sha256 . unHash)
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

checksum :: Hash -> Checksum
checksum = Checksum . sha256' . Hash . BS.map (+1) . unHash

encrypt :: Hash -> Hash -> EncryptedHash
encrypt (Hash bs1) (Hash bs2) = EncryptedHash $ Hash $ BS.packZipWith xor bs1 bs2

decrypt :: Hash -> EncryptedHash -> Hash
decrypt hashKey (EncryptedHash eHash) = unEncryptedHash $ encrypt hashKey eHash
 
abcHash :: Hash
abcHash = Hash $ decodeBase16Lenient bs
    where bs = "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"

sha256iterFFI2 :: Int -> Hash -> Hash
sha256iterFFI2 i hash = unsafePerformIO $ sha256iterFFI i hash

sha256iterFFI :: Int -> Hash -> IO Hash
sha256iterFFI i hash =
    do
        let bs' = BS.copy $ unHash hash
        unsafeUseAsCString bs' (c_sha256_iter i)
        pure $ Hash bs'

foreign import ccall safe "sha256_iter"
  c_sha256_iter :: Int -> CString-> IO ()