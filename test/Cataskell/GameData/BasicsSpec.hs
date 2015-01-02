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
    let p = Point { coord = (0, 0), position = Top }
    let p' = Point { coord = (0, -1), position = Bottom }
    let c = Blue
    let e = UndirectedEdge p p'
    describe "has an optional point value" $ do
      it "is Just 1 for a built settlement" $ do
        (fmap pointValue . getActualItem $ settlement (Just (p, c))) `shouldBe` Just 1
      it "is Nothing for an unbuilt settlement" $ do
        (fmap pointValue . getActualItem $ settlement Nothing) `shouldBe` Nothing
      it "is 2 for a built city" $ do
        (fmap pointValue . getActualItem $ city (Just (p, c))) `shouldBe` Just 2
      it "is Nothing for an unbuilt city" $ do
        (fmap pointValue . getActualItem $ city Nothing) `shouldBe` Nothing
      it "is 0 for a built road" $ do
        (fmap pointValue . getActualItem $ road (Just (e, c))) `shouldBe` Just 0
      it "is 1 for a built victory card" $ do
        (fmap pointValue . getActualItem $ devCard (Just VictoryPoint)) `shouldBe` Just 1
      it "is 0 for a built development card of another kind" $ do
        (fmap pointValue . getActualItem $ devCard (Just Knight)) `shouldBe` Just 0
  describe "A Bonus" $ do
    it "should be worth 2 points" $ do
      pointValue LongestRoad `shouldBe` 2
      pointValue LargestArmy `shouldBe` 2