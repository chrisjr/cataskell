module Cataskell.UtilSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Arbitrary()
import Cataskell.Util
import System.Random

instance Arbitrary StdGen where
  arbitrary = do
    seed <- arbitrary
    return $ mkStdGen seed

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "The Num (a,b) instance" $ do
    it "should add element-wise" $ property $
      let prop' (x1, y1) (x2, y2) = (x1 :: Int, y1 :: Int) + (x2 :: Int, y2 :: Int) == (x1+x2, y1+y2)
      in  prop'
  describe "windowed" $ do
    it "should make windows of equal size" $ property $
      \i lst -> let w = windowed (i :: Int) (lst :: [Int])
                in (length w >= 1) && (all (== i) $ map length w)
  describe "listToDuple" $ do
    it "should turn a list of two elements into a duple" $ do
      listToDuple [1, 2] `shouldBe` Just (1, 2)
    it "should turn anything else into Nothing" $ do
      listToDuple ([] :: [Int]) `shouldBe` Nothing
      listToDuple [1] `shouldBe` Nothing
      listToDuple [1, 2, 3] `shouldBe` Nothing