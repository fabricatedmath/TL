{-# LANGUAGE RankNTypes #-}

module TL.Util where

import Crypto.TL (HashMode, Hashable, shax86Mode, cryptoniteMode, shaGenericMode)

import Options.Applicative

data Mode = X86 | Generic | Cryptonite
    deriving Show

mode :: Parser Mode
mode = x86 <|> generic <|> cryptonite

x86 :: Parser Mode
x86 = flag' X86 (short 'x')

generic :: Parser Mode
generic = flag' Generic (short 'g')

cryptonite :: Parser Mode
cryptonite = flag' Cryptonite (short 'c')

fromBool :: a -> a -> Bool -> a
fromBool a1 _ True = a1
fromBool _ a2 False = a2

getFunc :: (forall a. Hashable a => HashMode a -> b) -> Mode -> b
getFunc f X86 = f shax86Mode
getFunc f Cryptonite = f cryptoniteMode
getFunc f Generic = f shaGenericMode