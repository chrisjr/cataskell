{-# OPTIONS_GHC -fno-warn-orphans #-}
module Cataskell.Util where

instance (Num a, Num b) => Num (a,b) where
  fromInteger x = (fromInteger x, fromInteger x)
  (a,b) + (a',b') = (a + a', b + b')
  (a,b) - (a',b') = (a - a', b - b')
  (a,b) * (a',b') = (a * a', b * b')
  negate (a,b) = (negate a, negate b)
  abs (a,b) = (abs a, abs b)
  signum (a,b) = (signum a, signum b)
 
windowed :: Int -> [a] -> [[a]]
windowed _ [] = []
windowed size ls@(_:xs) = 
  if length ls >= size 
  then (take size ls) : windowed size xs 
  else windowed size xs

listToDuple :: (Show a) => [a] -> Maybe (a, a)
listToDuple (x:y:[]) = Just (x, y)
listToDuple _ = Nothing