module Cataskell.GameData.LocationSpec (main, spec) where

import Test.Hspec
import Cataskell.GameData.Location

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "A Point" $ do
    it "has a hex coordinate and position" $ do
      let p = Point { coord = (0,0), position = Top }
      coord p `shouldBe` (0,0)
      position p `shouldBe` Top
  describe "An UndirectedEdge" $ do
    let p1 = Point { coord = (0,0), position = Top }
    let p2 = Point { coord = (0,0), position = Top }
    let e = UndirectedEdge { point1 = p1, point2 = p2 }
    it "should have two points" $ do
      point1 e `shouldBe` p1
      point2 e `shouldBe` p2
    it "should be equal to itself and an edge going the opposite way" $ do
      let e' = UndirectedEdge { point1 = p2, point2 = p1 }
      e `shouldBe` e'