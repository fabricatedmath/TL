{-# LANGUAGE ExistentialQuantification #-}

module Crypto.TL.Impls 
  ( shaModeArm, shaModeGeneric, shaModeNative, shaModeX86, shaModeCuda
  , getHashFuncs
  ) where

import Crypto.TL.Impls.Cuda

import Crypto.TL.Impls.Arm
import Crypto.TL.Impls.Generic
import Crypto.TL.Impls.Native
import Crypto.TL.Impls.X86
import Crypto.TL.Types

data HashBox = forall a. HasHashFunc a => HB (HashMode a)

getHashFuncs :: IO [(Char, String, Either String HashFunc)]
getHashFuncs = mapM f
  [ ('a', "Arm", HB shaModeArm)
  , ('g', "Generic", HB shaModeGeneric)
  , ('n', "Native", HB shaModeNative)
  , ('x', "x86", HB shaModeX86)
  ]
  where 
    f (c,s,HB a) = do
      ehashFunc <- getHashFunc a
      return (c,s,ehashFunc)


