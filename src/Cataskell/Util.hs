{-# OPTIONS_GHC -fno-warn-orphans #-}
module Cataskell.Util where

import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe)

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

iterate' :: (a -> a) -> a -> [a]
iterate' f x = x `seq` (x : iterate' f (f x))

findKeyValueWhere :: (k -> a -> Bool) -> Map.Map k a -> Maybe (k, a)
findKeyValueWhere f m = listToMaybe . Map.toList $ Map.filterWithKey f m

findKeyWhere :: (a -> Bool) -> Map.Map k a -> Maybe k
findKeyWhere f m = listToMaybe . Map.keys $ Map.filter f m

findValueWhere :: (a -> Bool) -> Map.Map k a -> Maybe a
findValueWhere f m = listToMaybe . Map.elems $ Map.filter f m

