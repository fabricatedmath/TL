{-# LANGUAGE ScopedTypeVariables #-}

module Crypto.TL.Util 
  ( stringifyHashRate, stringifyHash
  ) where

import Data.NumberLength
import Text.Printf

stringifyHashRate :: Double -> String
stringifyHashRate a = numString <> " " <> prefix <> "H/s"
  where
    ndigits = subtract 1 $ numDigits a
    place = ndigits `div` 3
    prefix = getUnitsPrefix place
    numString = printf "%.3f" (a / 1000 ^ place)

stringifyHash :: Integral a => a -> String
stringifyHash a' = numString <> " " <> prefix <> "hashes"
  where
    a :: Double = fromIntegral a'
    ndigits = subtract 1 $ numDigits a
    place = ndigits `div` 3
    prefix = getUnitsPrefixLong place
    numString = printf "%.3f" (a / 1000 ^ place)

numDigits :: RealFrac a => a -> Int
numDigits n = numberLength $ (truncate n :: Integer)

getUnitsPrefix :: Int -> String
getUnitsPrefix i = 
  case i of 
    0 -> ""
    1 -> "K"
    2 -> "M"
    3 -> "G"
    4 -> "T"
    5 -> "P"
    _ -> "error too much"

getUnitsPrefixLong :: Int -> String
getUnitsPrefixLong i = 
  case i of 
    0 -> ""
    1 -> "Kilo"
    2 -> "Mega"
    3 -> "Giga"
    4 -> "Tera"
    5 -> "Peta"
    _ -> "error too much"
