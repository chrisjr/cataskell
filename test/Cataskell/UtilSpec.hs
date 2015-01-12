{-# LANGUAGE FlexibleInstances #-}

module Cataskell.UtilSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Arbitrary()
import Cataskell.Util
import System.Random
import Data.Maybe
import qualified Data.Map.Strict as Map
import Control.Applicative ((<$>))

instance Arbitrary StdGen where
  arbitrary = do
    seed <- arbitrary
    return $ mkStdGen seed

instance Arbitrary (Map.Map Int [Int]) where
  arbitrary = do
    keys <- listOf arbitrary
    values <- listOf $ listOf1 arbitrary
    return $ Map.fromList $ zip keys values
  shrink m = Map.fromList <$> shrink (Map.toList m)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "The Num (a,b) instance" $
    it "should add element-wise" $ property $
      let prop' (x1, y1) (x2, y2) = (x1 :: Int, y1 :: Int) + (x2 :: Int, y2 :: Int) == (x1+x2, y1+y2)
      in  prop'
  describe "windowed" $
    it "should make windows of equal size" $ property $
      let prop' (NonNegative i) lst = all (== i) . map length $ windowed i (lst :: [Int])
      in prop'
  describe "listToDuple" $ do
    it "should turn a list of two elements into a duple" $
      listToDuple [1, 2] `shouldBe` Just (1, 2)
    it "should turn anything else into Nothing" $ do
      listToDuple ([] :: [Int]) `shouldBe` Nothing
      listToDuple [1] `shouldBe` Nothing
      listToDuple [1, 2, 3] `shouldBe` Nothing
  describe "iterate'" $
    it "should act as a strict version of iterate" $ do
      let n = 1000000
      iterate' (+1) 0 !! n `shouldBe` n
  
  describe "counts" $
    it "should return Map a Int of frequencies" $ do
      counts [1,1,3,5] `shouldBe` Map.fromList [(1,2),(3,1),(5,1)]

  let getFirst m = let f' = listToMaybe $ Map.toList m
                       k' = fst `fmap` f'
                       v' = snd `fmap` f'
                   in (f', k', v')

  describe "findKeyValueWhere" $
    it "should return Just (key, value) if a (key, value) matching the predicate is found, else Nothing" $ property $
      \m -> let (first', k, v) = getFirst (m :: Map.Map Int [Int])
            in isJust first' ==>
               let k' = fromJust k
                   v' = fromJust v
                   hasKV = findKeyValueWhere (\k v -> k == k' && v == v') m == Just (k', v')
                   fakeKV = ((1 + maximum (Map.keys m)), [1 + maximum (concat $ Map.elems m)])
                   noKVforFakeKV = isNothing (findKeyValueWhere (\k v -> (k,v) == fakeKV) m)
               in  hasKV && noKVforFakeKV
  describe "findKeyWhere" $
    it "should return Just key if a value matching the predicate is found, else Nothing" $ property $
      \m -> let (first', k, v) = getFirst (m :: Map.Map Int [Int])
            in isJust first' ==>
               let k' = fromJust k
                   v' = head $ fromJust v
                   isK = (findKeyWhere (elem v') m) == Just k'
                   fakeV = 1 + maximum (concat $ Map.elems m)
                   noKforFakeV = isNothing (findKeyWhere (elem fakeV) m)
               in  isK && noKforFakeV
  describe "findValueWhere" $
    it "should return Just value if a value matching the predicate is found, else Nothing" $ property $
      \m -> let (first', _, v) = getFirst (m :: Map.Map Int [Int])
            in isJust first' ==>
               let v' = fromJust v
                   isV = (findValueWhere (elem (head v')) m) == (Just v')
                   fakeV = 1 + maximum (concat $ Map.elems m)
                   noVforFakeV = isNothing (findValueWhere (elem fakeV) m)
               in  isV && noVforFakeV
