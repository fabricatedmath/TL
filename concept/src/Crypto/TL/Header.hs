module Crypto.TL.Header where

import Control.Monad (when)
import Data.Serialize

import Crypto.TL.Primitives.Chain
import Crypto.TL.Primitives.Hash

data Header = Header ChainHead

instance Serialize Header where
  put (Header chainHead) = do
    put magicHash
    put chainHead
    
  get = do
    magicHash' <- get
    when (magicHash' /= magicHash) $ fail "Failed to match TLA Magic Hash"
    chainHead <- get
    return $ Header chainHead

magicHash :: Hash
magicHash = hashFlipEndian $ Hash
  0xad830d99 0x06e62640 0x93497b5e 0x51b56ff5
  0x6c798088 0x4a37aeb3 0x289352fa 0x702da3b9