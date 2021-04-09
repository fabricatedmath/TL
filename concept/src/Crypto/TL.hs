module Crypto.TL
    ( module Crypto.TL.Chain
    , module Crypto.TL.Chain.Parallel
    , module Crypto.TL.Primitives
    ) where

import Crypto.TL.Chain
import Crypto.TL.Chain.Parallel (createChainParallel)
import Crypto.TL.Primitives 
    ( hashDefault
    , Slow, SlowMode, slowMode
    , Fast, FastMode, fastMode
    , Hash, Hashable(..), HashMode
    )