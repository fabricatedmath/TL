{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Crypto.TL.Archive.Header 
  ( TLAHeader(..), MagicHash(..), TLAFileName(..), TLAText(..)
  , tlaHeader, tlaGetFileName, tlaGetChainHead
  , magicHash, tlaShaHashMethod, tlaCurrentVersion
  ) where

import Control.Monad (replicateM, when)
import Data.Serialize
import Data.Word (Word8)

import System.FilePath.Posix (takeFileName)

import Crypto.TL.Primitives.Chain
import Crypto.TL.Primitives.Hash
import Crypto.TL.Primitives.ShortHash

tlaHeader :: ChainHead -> FilePath -> TLAHeader
tlaHeader chainHead fp = 
  let tlaFileName = TLAFileName $ TLAText $ takeFileName fp
  in TLAHeader MagicHash tlaCurrentVersion tlaShaHashMethod chainHead tlaFileName

tlaGetChainHead :: TLAHeader -> ChainHead
tlaGetChainHead (TLAHeader _ _ _ chainHead _) = chainHead

tlaGetFileName :: TLAHeader -> String
tlaGetFileName (TLAHeader _ _ _ _ (TLAFileName (TLAText fileName))) = fileName

data TLAHeader = TLAHeader MagicHash TLAVersion TLAHashMethod ChainHead TLAFileName
  deriving (Eq, Show)

data TLAVersion = TLAVersion Word8 Word8
  deriving (Eq, Show)

tlaCurrentVersion :: TLAVersion
tlaCurrentVersion = TLAVersion 0 1

instance Serialize TLAVersion where
  put (TLAVersion major minor) = put major >> put minor
  get = TLAVersion <$> get <*> get

newtype TLAHashMethod = TLAHashMethod TLAText
  deriving (Eq, Serialize, Show)

tlaShaHashMethod :: TLAHashMethod
tlaShaHashMethod = TLAHashMethod $ TLAText $ "SHA256"

newtype TLAFileName = TLAFileName TLAText
  deriving (Eq, Serialize, Show)

newtype TLAText = TLAText String
  deriving (Eq, Show)

instance Serialize TLAText where
  put (TLAText s) = do
    putWord32le $ fromIntegral $ length s
    mapM_ put s
  get = do
    len <- fromIntegral <$> getWord32le
    TLAText <$> replicateM len get

instance Serialize TLAHeader where
  put (TLAHeader tlaMagicHash tlaVersion tlaHashMethod chainHead tlaFileName) = do
    put tlaMagicHash
    put tlaVersion
    put tlaHashMethod
    put chainHead
    put tlaFileName
  get = TLAHeader <$> get <*> get <*> get <*> get <*> get

data MagicHash = MagicHash
  deriving (Eq, Show)

instance Serialize MagicHash where
  put _ = put shortMagicHash
  get = do
    shortMagicHash' <- get
    when (shortMagicHash' /= shortMagicHash) $ 
      fail $ "Failed to match TLA Magic Hash: " <> show shortMagicHash' 
          <> " with the expected: " <> show shortMagicHash 
    return MagicHash

shortMagicHash :: ShortHash
shortMagicHash = hashToShortHash magicHash

magicHash :: Hash
magicHash = hashFlipEndian $ Hash
  0xad830d99 0x06e62640 0x93497b5e 0x51b56ff5
  0x6c798088 0x4a37aeb3 0x289352fa 0x702da3b9