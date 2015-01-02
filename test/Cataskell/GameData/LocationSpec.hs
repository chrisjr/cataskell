module Cataskell.GameData.LocationSpec (main, spec) where

import Test.Hspec
import Data.List (sort)
import Cataskell.GameData.Location
import qualified Data.Map.Strict as Map

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "A HexCoord" $ do
    let aroundNegOneTwo = [((-1),1), (0,1), (0,2),((-1),3),((-2),3), ((-2),2)]
    let n = [(0, 0), (1, 0), (1, 1), (1, 2), (0, 3)]
    let notAroundNegOneTwo = n ++ [((-1), 4), ((-2), 4), ((-3), 4), ((-3), (-3)), ((-3), 2), ((-2), 1)]
    it "can be tested to be in a certain radius" $ do
      let check = withinHexRadius 1 (-1, 2)
      all check aroundNegOneTwo `shouldBe` True
      any check notAroundNegOneTwo `shouldBe` False
    it "can get neighboring coordinates that are actually on the board" $ do
      let ns = sort $ neighborCoords (-1, 2)
      let ns' = [(-2,2), (-1, 1), (0, 1), (0, 2)]
      ns `shouldBe` (sort ns')
    it "should have neighborhoods" $ do
      let h = hexNeighborhoods
      Map.size h `shouldBe` 19
      let neighborhoodSizes = map snd . Map.toList $ Map.map length h
      all (\x -> x == 3 || x == 4 || x == 6) neighborhoodSizes `shouldBe` True
      (length $ (Map.!) h (mkCenter (-2, 0))) `shouldBe` 3
  describe "A Point" $ do
    it "has a hex coordinate and position" $ do
      let p = Point { coord = (0,0), position = Top }
      coord p `shouldBe` (0,0)
      position p `shouldBe` Top
    it "can be made from a hex coordinate" $ do
      mkCenter (0, 0) `shouldBe` Point { coord = (0,0), position = Center }
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