module Cataskell.UtilSpec (main, spec) where

import Test.Hspec
import Cataskell.Util

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Num (a,b) instance" $ do
    it "should add element-wise" $ do
      ((1, 2) + (3, 4)) `shouldBe` (4, 6)
  describe "windowed" $ do
    it "should make windows of equal size" $ do
      windowed 1 [1, 2, 3] `shouldBe` [[1], [2], [3]]
      windowed 2 [1, 2, 3] `shouldBe` [[1, 2], [2, 3]]
      windowed 3 [1, 2, 3] `shouldBe` [[1, 2, 3]]
  describe "listToDuple" $ do
    it "should turn a list of two elements into a duple" $ do
      listToDuple [1, 2] `shouldBe` Just (1, 2)
    it "should turn anything else into Nothing" $ do
      listToDuple ([] :: [Int]) `shouldBe` Nothing
      listToDuple [1] `shouldBe` Nothing
      listToDuple [1, 2, 3] `shouldBe` Nothing