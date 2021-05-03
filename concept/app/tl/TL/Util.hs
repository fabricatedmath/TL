{-# LANGUAGE RankNTypes #-}

module TL.Util where

import Crypto.TL (Mode(..))

import Options.Applicative

mode :: Parser Mode
mode = x86 <|> generic <|> cryptonite

x86 :: Parser Mode
x86 = flag' X86 (short 'x' <> help "Use specialized x86 with sse4.1 and sha extensions (Newer cpus, very fast)")

generic :: Parser Mode
generic = flag' Generic (short 'g' <> help "Use generic sha (very slow)")

cryptonite :: Parser Mode
cryptonite = flag' Cryptonite (short 'c' <> help "Use Haskell library implementation (Slow, faster than generic)")

fromBool :: a -> a -> Bool -> a
fromBool a1 _ True = a1
fromBool _ a2 False = a2
