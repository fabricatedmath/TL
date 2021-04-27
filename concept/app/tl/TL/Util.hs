module TL.Util where

data Mode = Slow | Fast
    deriving Show

fromBool :: a -> a -> Bool -> a
fromBool a1 _ True = a1
fromBool _ a2 False = a2