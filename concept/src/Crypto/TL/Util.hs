module Crypto.TL.Util where

import Data.NumberLength
import Text.Printf

stringifyHashRate :: Double -> String
stringifyHashRate a = 
  let
    ndigits = subtract 1 $ numDigits a
    place = ndigits `div` 3
    prefix = getUnitsPrefix place
    numString = printf "%.3f" (a / 1000 ^ place)
  in numString <> " " <> prefix <> "H/s"

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