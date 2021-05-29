{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Crypto.TL.Archive.Header where

import Control.Monad (replicateM, when)
import Data.Serialize

import Crypto.TL.Primitives.Chain
import Crypto.TL.Primitives.Hash
import Crypto.TL.Primitives.ShortHash

data TLAHeader = TLAHeader MagicHash TLAHashMethod ChainHead TLAFileName
  deriving (Eq, Show)

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
  put (TLAHeader tlahash tlahashMethod chainHead tlaFileName) = do
    put tlahash
    put tlahashMethod
    put chainHead
    put tlaFileName
  get = TLAHeader <$> get <*> get <*> get <*> get

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