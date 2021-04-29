{-# LANGUAGE  RankNTypes #-}

module Crypto.TL.Hashing 
  ( cryptoniteMode
  , shaGenericMode
  , shax86Mode
  , Mode(..), getHashingFunc
  )
where

import Crypto.TL.Hashing.Cryptonite
import Crypto.TL.Hashing.Generic
import Crypto.TL.Hashing.X86

import Crypto.TL.Primitives

data Mode = X86 | Generic | Cryptonite
    deriving Show

getHashingFunc :: (forall a. Hashable a => HashMode a -> b) -> Mode -> b
getHashingFunc f X86 = f shax86Mode
getHashingFunc f Cryptonite = f cryptoniteMode
getHashingFunc f Generic = f shaGenericMode