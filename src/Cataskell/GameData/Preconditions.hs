{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Cataskell.GameData.Preconditions where

import Data.Typeable
import Data.Either

instance (Typeable a, Typeable b) => Show (a -> b) where
  show _ = show $ typeOf (undefined :: a -> b)

data (Show a) => Precondition a
  = Precondition { predicate :: a -> Bool
                 , label :: String
                 } deriving (Show)

-- | Returns Right True if predicate holds, Left (string label) otherwise
check :: (Show a) => Precondition a -> a -> Either String Bool
check p x = if predicate p x then Right True else Left (label p)

-- | Returns True if predicate holds, throws an exception otherwise
checkUnsafe :: (Show a) => Precondition a -> a -> Bool
checkUnsafe p x = either error id $ check p x

-- | Checks a list of predicates and returns Right True or Left [failed]
checkAll :: (Show a) => [Precondition a] -> a -> Either [String] Bool 
checkAll ps x
  = let results = map (`check` x) ps
        failures = lefts results
    in if null failures
          then Right True
          else Left failures

-- | Checks a list of predicates and returns True or throws an exception containing all failures
checkAllUnsafe :: (Show a) => [Precondition a] -> a -> Bool
checkAllUnsafe ps x = either (error . unlines) id $ checkAll ps x
