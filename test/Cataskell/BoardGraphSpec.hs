module Cataskell.BoardGraphSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Cataskell.BoardGraph
import Cataskell.GameData.Location
import qualified Data.Set as Set
import Data.Tuple (swap)
import Data.Graph.Inductive.Basic (isSimple, elfilter)
import Data.Graph.Inductive.Graph (empty, labEdges, labNodes)
import Data.Graph.Inductive.PatriciaTree()

instance Arbitrary UndirectedEdge where
  arbitrary = elements (Set.toList allEdges)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
  let isPos pos x = position x == pos
  let isIntersection x = isPos Top x || isPos Bottom x
  
  describe "A hex graph" $ do
    let hex = addHex empty . toCenter $ mkCenter (0,0)
    it "has 7 vertices total" $ do
      countNodes (\_ -> True) hex `shouldBe` (7 :: Int)
    it "has 6 outer vertices" $ do
      countNodes isIntersection hex `shouldBe` (6 :: Int)
    it "is connected around the outside" $ do
      let edges = labEdges $ elfilter (\x -> edgeType x == Between) hex
      length edges `shouldBe` (12 :: Int)
      let pairs = Set.fromList $ map (\(x,y,_) -> (x,y)) edges
      let oneway = Set.fromList [(2,3), (3,4), (4,5), (5,6), (6,7), (7,2)] 
      pairs `shouldBe` Set.union oneway (Set.map swap oneway)

  describe "A board graph" $ do
    it "has 19 hexes" $ do
      let centers = filter (\x -> position x == Center) . map snd $ labNodes boardGraph
      length centers `shouldBe` (19 :: Int)
      let cells = top' ++ bottom'
            where top' = zip [0..] (r 3 (-2)) ++ zip [-1..] (r 4 (-1))
                  bottom' = zip f2 (r 5 0) ++ zip f2 (r 4 1) ++ zip f2 (r 3 2)
                  r = replicate
                  f2 = [-2..]
      (Set.fromList $ map coord centers) `shouldBe` Set.fromList cells
    it "has 54 intersections" $ do
      countNodes isIntersection boardGraph `shouldBe` (54 :: Int)
      countNodes (isPos Top) boardGraph `shouldBe` (27 :: Int)
      countNodes (isPos Bottom) boardGraph `shouldBe` (27 :: Int)
    it "has 6 top/bottom points in each center's neighborhood" $ do
      let centers = map mkCenter hexCoords
      let neighborhoods = map (Set.filter isIntersection . neighborPoints boardGraph) centers
      neighborhoods `shouldSatisfy` all ((== 6) . Set.size)
    it "should return the top/bottom neighbors of a specified point" $ do
      let hC = (0, 0)
      let p = Point hC Center
      let pNeighbors = Set.fromList $ tail . fst . mkHexPointsAndEdges $ toCenter p
      Set.filter isIntersection (neighborPoints boardGraph p) `shouldBe` pNeighbors
    it "should be linked between centers" $ do
      let centers = map mkCenter hexCoords
      let centerNeighborPoints = Set.fromList . map (Set.fromList . map mkCenter . neighborCoords) $ hexCoords
      let centerNeighborhoods = Set.fromList $ map (neighborPoints centerConnections) centers
      centerNeighborhoods `shouldBe` centerNeighborPoints
    it "has no loops" $ isSimple boardGraph
  describe "roadGraph" $ do
    it "should be built from a set of edges" $ do
      let es = Set.fromList [ mkEdge (0,0, Top) (1,-1, Bottom)
                            , mkEdge (1,-1,Bottom) (1,0,Top)
                            , mkEdge (1,0,Top) (2, -1, Bottom)
                            , mkEdge (0,2,Bottom) (0,3,Top)]
      let r = roadGraph es (Set.singleton (Point (1,0) Top))
      length (labNodes r) `shouldBe` 4
      length (labEdges r) `shouldBe` 1
    it "should not choke on undirected cycles" $ do
      let es = Set.fromList [ mkEdge (0,0, Top) (1,-1, Bottom)
                            , mkEdge (1,-1,Bottom) (0,1,Top)
                            , mkEdge (0,1,Top) (0, 0, Bottom)
                            , mkEdge (0,0, Bottom) (-1,1,Top)
                            , mkEdge (-1,1,Top) (0,-1,Bottom)
                            , mkEdge (0,-1,Bottom) (0,0,Top)]
      let r = roadGraph es Set.empty
      labNodes r `shouldSatisfy` (== 6) . length
      labEdges r `shouldSatisfy` (== 6) . length
