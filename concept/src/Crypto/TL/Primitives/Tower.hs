module Crypto.TL.Primitives.Tower 
  ( Tower(..)
  ) where

import Crypto.TL.Primitives.Hash

data Tower = 
  Tower
  { towerStart :: !Hash
  , towerEnd :: !Hash
  } deriving (Eq, Show)