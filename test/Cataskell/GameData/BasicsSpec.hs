module Cataskell.GameData.BasicsSpec (main, spec) where

import Test.Hspec
import Cataskell.GameData.Basics
import Cataskell.GameData.Location

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "A Construct" $ do
    it "can be a settlement, city, road, or development card" $ do
      let s = unbuilt settlement
      let c = unbuilt city
      let r = unbuilt road
      let d = unbuilt devCard
      s `shouldBe` Potential (HabitationToBe Settlement)
      c `shouldBe` Potential (HabitationToBe City)
      r `shouldBe` Potential (RoadToBe Road)
      d `shouldBe` DevCard
    it "should have a point value" $ do
      let p = Point { coord = (0, 0), position = Top }
      let p' = Point { coord = (0, -1), position = Bottom }
      let c = Blue
      let e = UndirectedEdge p p'
      (fmap pointValue . getActualItem $ settlement (Just (p, c))) `shouldBe` Just 1
      (fmap pointValue . getActualItem $ settlement Nothing) `shouldBe` Nothing
      (fmap pointValue . getActualItem $ city (Just (p, c))) `shouldBe` Just 2
      (fmap pointValue . getActualItem $ city Nothing) `shouldBe` Nothing
      (fmap pointValue . getActualItem $ road (Just (e, c))) `shouldBe` Just 0
      (fmap pointValue . getActualItem $ devCard (Just VictoryPoint)) `shouldBe` Just 1
      (fmap pointValue . getActualItem $ devCard (Just Knight)) `shouldBe` Nothing
  describe "A Bonus" $ do
    it "should be worth 2 points" $ do
      pointValue LongestRoad `shouldBe` 2
      pointValue LargestArmy `shouldBe` 2