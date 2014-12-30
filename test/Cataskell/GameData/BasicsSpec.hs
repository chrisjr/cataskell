module Cataskell.GameData.BasicsSpec where

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
      s `shouldBe` Building (Habitation Settlement Nothing)
      c `shouldBe` Building (Habitation City Nothing)
      r `shouldBe` Building (Road Nothing)
      d `shouldBe` DevCard Nothing
    it "should have a point value" $ do
      let p = Point { coord = (0, 0), position = Top }
      let p' = Point { coord = (0, -1), position = Bottom }
      let e = UndirectedEdge p p'
      (pointValue $ settlement (Just p)) `shouldBe` 1
      (pointValue $ settlement Nothing) `shouldBe` 0
      (pointValue $ city (Just p)) `shouldBe` 2
      (pointValue $ city Nothing) `shouldBe` 0
      (pointValue $ road (Just e)) `shouldBe` 0
      (pointValue $ devCard (Just VictoryPoint)) `shouldBe` 1
      (pointValue $ devCard (Just Knight)) `shouldBe` 0
