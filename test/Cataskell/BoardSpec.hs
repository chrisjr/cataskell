module Cataskell.BoardSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Cataskell.Board
import Cataskell.GameData.Location
import Data.List
import Data.Maybe
import Data.Tuple (swap)
import Data.Graph.Inductive.Basic (elfilter)
import Data.Graph.Inductive.Graph (empty, labEdges, labNodes, neighbors)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  let isPos pos x = position x == pos
  let isIntersection x = isPos Top x || isPos Bottom x
  
  describe "A hex graph" $ do
    let hex = addHex (0,0) empty
    it "has 7 vertices total" $ do
      countNodes (\_ -> True) hex `shouldBe` (7 :: Int)
    it "has 6 outer vertices" $ do
      countNodes isIntersection hex `shouldBe` (6 :: Int)
    it "is connected around the outside" $ do
      let edges = labEdges $ elfilter (\x -> edgeType x == Between) hex
      length edges `shouldBe` (12 :: Int)
      let pairs = sort $ map (\(x,y,_) -> (x,y)) edges
      let oneway = [(2,3), (3,4), (4,5), (5,6), (6,7), (7,2)] 
      pairs `shouldBe` sort (oneway ++ map swap oneway)
  
  describe "A board graph" $ do
    it "has 19 hexes" $ do
      let centers = filter (\x -> position x == Center) . map snd $ labNodes boardGraph
      length centers `shouldBe` (19 :: Int)
      let cells = top' ++ bottom'
            where top' = zip [0..] (r 3 (-2)) ++ zip [-1..] (r 4 (-1))
                  bottom' = zip f2 (r 5 0) ++ zip f2 (r 4 1) ++ zip f2 (r 3 2)
                  r = replicate
                  f2 = [-2..]
      (sort $ map coord centers) `shouldBe` sort cells
    it "has 54 intersections" $ do
      countNodes isIntersection boardGraph `shouldBe` (54 :: Int)
      countNodes (isPos Top) boardGraph `shouldBe` (27 :: Int)
      countNodes (isPos Bottom) boardGraph `shouldBe` (27 :: Int)
    it "has 6 points in each center's neighborhood" $ do
      let centers = map (\x -> Point { coord = x, position = Center }) hexes
      let centerNodes = mapMaybe (\x -> getNodeMaybe x boardGraph) centers
      let neighborhoods = map (nub . neighbors boardGraph) centerNodes
      all (\x -> length x == 6) neighborhoods `shouldBe` True
  
  describe "A board" $ do
    it "has a randomly generated set of terrains" $ do
      error "Not yet implemented"
      True `shouldBe` False      
    it "has no high-value terrains (6 or 8) next to each other" $ do
      error "nope"
      True `shouldBe` False
