module Crypto.TL.Bulk where

import Control.Monad (replicateM)

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Serialize (runGet, get, runPut, put)

import Crypto.TL.Types (Hash)

toPacked :: [Hash] -> ByteString
toPacked = runPut . mapM_ put

toUnpacked :: ByteString -> [Hash]
toUnpacked bs = hashes
  where
    Right hashes = runGet (replicateM numHashes get) bs
    numHashes = BS.length bs `div` 32
    