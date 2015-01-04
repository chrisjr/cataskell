module Cataskell.GameData.BasicsSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Data.Maybe (isJust)
import Control.Lens hiding (elements)
import Cataskell.GameData.Basics
import Cataskell.GameData.Location

import Cataskell.GameData.LocationSpec()
import Cataskell.BoardGraphSpec()

instance Arbitrary Item where
  arbitrary = undefined

instance Arbitrary Color where
  arbitrary = elements [Red, White, Orange, Blue]

instance Arbitrary Construct where
  arbitrary = do
    p <- arbitrary
    e <- arbitrary
    c <- arbitrary
    elements [built . settlement $ Just (p, c), built . city $ Just (p,c), built . road $ Just (e,c)]

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "A Item" $ do
    it "can be a settlement, city, road, or development card" $ do
      let s = unbuilt settlement
      let c = unbuilt city
      let r = unbuilt road
      let d = unbuilt devCard
      s `shouldBe` Potential (H Settlement)
      c `shouldBe` Potential (H City)
      r `shouldBe` Potential Road
      d `shouldBe` Potential DevelopmentCard

    it "should have a color if it is a built settlement, road, or city" $ property $
      \construct -> let bldg = construct :: Construct
                        color' = color bldg
                    in  color' `elem` [Red, Blue, Orange, White]
    let p = Point { coord = (0, 0), position = Top }
    let p' = Point { coord = (0, -1), position = Bottom }
    let c = Blue
    let e = UndirectedEdge p p'
    describe "has a point value" $ do
      it "is 1 for a built settlement" $ do
        (pointValue $ settlement (Just (p, c))) `shouldBe` 1
      it "is 0 for an unbuilt settlement" $ do
        (pointValue $ settlement Nothing) `shouldBe` 0
      it "is 2 for a built city" $ do
        (pointValue $ city (Just (p, c))) `shouldBe` 2
      it "is 0 for an unbuilt city" $ do
        (pointValue $ city Nothing) `shouldBe` 0
      it "is 0 for a built road" $ do
        (pointValue $ road (Just (e, c))) `shouldBe` 0
      it "is 1 for a built victory card" $ do
        (pointValue $ devCard (Just VictoryPoint)) `shouldBe` 1
      it "is 0 for a built development card of another kind" $ do
        (pointValue $ devCard (Just Knight)) `shouldBe` 0
  describe "A Bonus" $ do
    it "should be worth 2 points" $ do
      pointValue LongestRoad `shouldBe` 2
      pointValue LargestArmy `shouldBe` 2