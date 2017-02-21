{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Cataskell.GameData.BoardSpec (main, spec, blueRoadsLonger, whiteRoads) where

import Test.Hspec
import Control.Exception
import Control.DeepSeq
import Test.QuickCheck
import Cataskell.GameData.Board
import Cataskell.GameData.Basics
import Cataskell.GameData.Resources
import Cataskell.GameData.Location hiding (mkEdge)
import qualified Cataskell.GameData.Location as L (mkEdge)
import Cataskell.Util
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (find)
import Data.Monoid (mempty)
import Data.Maybe
import Control.Lens hiding (elements)
import Control.Monad
import Control.Monad.Random
import Control.Applicative ((<$>), (<*>))

import Data.Graph.Inductive hiding (context)
import Data.Graph.Inductive.Dot
import Cataskell.GameData.LocationSpec()
import Cataskell.GameData.BasicsSpec ()
import Cataskell.UtilSpec() -- for Arbitrary StdGen instance

instance NFData Terrain
instance NFData ResourceCount
instance NFData HexCenter

instance Arbitrary HexCenter where
  arbitrary = do
    terrain' <- elements (Desert:terrains)
    roll' <- if terrain' == Desert then elements [7] else elements rolls
    return $ mkHexCenter terrain' roll'

instance Arbitrary HexMap where
  arbitrary = do
    stdGen <- (arbitrary :: Gen StdGen)
    return $ evalRand newHexMap stdGen
  shrink m = HexMap . Map.fromList <$> shrink (Map.toList $ unHexMap m)

instance Arbitrary OnEdge where
  arbitrary = OnEdge <$> arbitrary <*> arbitrary

instance Arbitrary OnPoint where
  arbitrary = OnPoint <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary RoadMap where
  arbitrary = do
    roads' <- listOf (arbitrary :: Gen OnEdge)
    let withRoads = Map.fromList $ map (\r -> (r^.edge, Just r)) roads'
    return $ under (_Unwrapping' RoadMap) (Map.union withRoads) emptyRoadMap
  shrink m = RoadMap . Map.fromList <$> shrink (Map.toList $ unRoadMap m)

instance Arbitrary BuildingMap where
  arbitrary = do
    buildings' <- listOf (arbitrary :: Gen OnPoint)
    let withBuildings = Map.fromList $ map (\b -> (b^.point, Just b)) buildings'
    return $ under (_Unwrapping' BuildingMap) (Map.union withBuildings) emptyBuildingMap
  shrink m = BuildingMap . Map.fromList <$> shrink (Map.toList $ unBuildingMap m)

instance Arbitrary Harbor where
  arbitrary = elements harborTypes

instance Arbitrary HarborMap where
  arbitrary = do
    stdGen <- (arbitrary :: Gen StdGen)
    return $ evalRand newHarborMap stdGen
  shrink m = HarborMap . Map.fromList <$> shrink (Map.toList $ unHarborMap m)

instance Arbitrary Board where
  arbitrary = do
    stdGen <- (arbitrary :: Gen StdGen)
    return $ evalRand newBoard stdGen
  shrink b = tail $ Board <$> shrink' (_hexes b) 
                          <*> shrink' (_roads b)
                          <*> shrink' (_buildings b)
                          <*> shrink' (_harbors b)
    where shrink' a = a : shrink a

-- | Road Test board
newtype RTBoard = RT Board
  deriving (Eq, Ord, Show, Read)

fromRTBoard :: RTBoard -> Board
fromRTBoard (RT b) = b

nonceRand :: StdGen
nonceRand = mkStdGen 0

instance Arbitrary RTBoard where
  arbitrary = do
    let board' = evalRand newBoard nonceRand
    r <- elements [blueRoads, blueRoads2]
    return . RT $ board' { _roads = r, _buildings = interruptSettlement (Point (1,-2) Top) }
  shrink rt = map RT $ tail $ Board <$> shrink' (_hexes b)
                                    <*> [under (_Unwrapping' RoadMap) (Map.filter isJust) (_roads b)]
                                    <*> [under (_Unwrapping' BuildingMap) (Map.filter isJust) (_buildings b)]
                                    <*> shrink' (_harbors b)
    where b = fromRTBoard rt
          shrink' a = a : shrink a

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
  hexCenterSpec
  hexMapSpec
  buildingMapSpec
  functionsSpec

hexCenterSpec :: Spec
hexCenterSpec =
  describe "A HexCenter" $ do
    it "has a terrain type and accompanying resource" $ do
      let hex = mkHexCenter Mountain 2
      _resource hex `shouldBe` mempty { ore = 1 }
    it "cannot be made with Desert && roll != 7, or !Desert && roll == 7" $ do
      _terrain (mkHexCenter Desert 7) `shouldBe` Desert
      (evaluate . force) (mkHexCenter Desert 5) `shouldThrow` (const True :: Selector AssertionFailed)
      (evaluate . force) (mkHexCenter Mountain 7) `shouldThrow` (const True :: Selector AssertionFailed)
    it "can be generated satisfying this requirement" $ property $
      \hc -> (_terrain (hc :: HexCenter) == Desert) == (_roll hc == 7)

hexMapSpec :: Spec
hexMapSpec = parallel $ do
  describe "A HexMap" $ do
    it "should create a randomly generated set of terrains and rolls" $ property $
      \(HexMap hexMap) -> Map.size hexMap == 19
    it "should have one 2, one 7, one 12, and two of everything else" $ property $
      let rollCounts h = counts . map _roll $ Map.elems h
          a h = rollCounts h Map.! 2 == 1
          b h = rollCounts h Map.! 7 == 1
          c h = rollCounts h Map.! 12 == 1
          d h = all (== 2) $ map ((Map.!) (rollCounts h)) [3..6]
          e h = all (== 2) $ map ((Map.!) (rollCounts h)) [8..11]
      in  \(HexMap hexMap) -> all ($ hexMap) [a,b,c,d,e]
    it "should have 3 hills, 4 pastures, 3 mountains, 4 fields, 4 forests, and 1 desert" $ property $
      \(HexMap hexMap) ->
        let terrainCounts = counts . map _terrain $ Map.elems hexMap
            h  = terrainCounts Map.! Hill     == 3
            p  = terrainCounts Map.! Pasture  == 4
            m  = terrainCounts Map.! Mountain == 3
            fi = terrainCounts Map.! Field    == 4
            fo = terrainCounts Map.! Forest   == 4
            d  = terrainCounts Map.! Desert   == 1
        in h && p && m && fi && fo && d
    it "should have no high-value terrains (6 or 8) next to each other" $ property $ \hexMap ->
      checkHexNeighbors hexMap
  describe "the Desert" $
    it "should have roll equal to 7" $ property $
      \(HexMap hexMap) ->
        let desertHex = head . filter ((== Desert) . _terrain) $ Map.elems hexMap
        in _roll desertHex == 7

buildingMapSpec :: Spec
buildingMapSpec = parallel $ do
  describe "A BuildingMap" $ do
    it "should have 54 keys" $
      Map.size (unBuildingMap emptyBuildingMap) `shouldBe` 54
    it "should start off empty" $
      Map.elems (unBuildingMap emptyBuildingMap) `shouldSatisfy` all (== Nothing)
  describe "A RoadMap" $ do
    it "should have 72 keys" $
      Map.size (unRoadMap emptyRoadMap) `shouldBe` 72
    it "should start off empty" $
      Map.elems (unRoadMap emptyRoadMap) `shouldSatisfy` all (== Nothing)
  describe "A Board" $ do
    it "should start off with no buildings and no roads" $ property $
      \board -> view buildings (board :: Board) == emptyBuildingMap
    it "can be queried for open points" $ property $
      \board -> Set.size (freePoints (board :: Board)) == 54
    it "can be queried for open points after building" $ property $
      \p board -> let bldg = built . settlement $ Just ((p :: Point), Blue)
                      board' = build bldg (board :: Board)
                      l = Set.size (freePoints board')
                  in  l == 51 || l == 50 -- at least 3 points now off limits, possibly 4
    it "can be queried for colors affected by a roll" $ do
      let board = evalRand newBoard $ mkStdGen 0
      -- print $ Map.filter ((== 6) . roll) (board ^. hexes)
      let bldg = built . settlement $ Just ((Point (0,0) Top), Blue)
      let board' = build bldg board
      let res' = allResourcesFromRoll 6 board'
      res' `shouldBe` Map.singleton Blue mempty { ore = 1 }

getE :: Construct -> UndirectedEdge
getE (Roadway (OnEdge e _)) = e 
getE _ = error "not a road"

mkRoadMap :: [UndirectedEdge] -> Color -> RoadMap
mkRoadMap es' color' = RoadMap . Map.fromList $ zip es' (map (\e -> Just $ OnEdge e color') es')

interruptSettlement :: Point -> BuildingMap
interruptSettlement p = BuildingMap $ Map.singleton p (Just $ OnPoint p Red Settlement)

blueRoads :: RoadMap
blueRoads = let ps = [ Point (0, -3) Bottom
                     , Point (0,-2) Top
                     , Point (1,-3) Bottom
                     , Point (1,-2) Top]
                es = map dupleToEdge . mapMaybe listToDuple $ windowed 2 ps
            in mkRoadMap es Blue

blueRoadsLonger :: RoadMap
blueRoadsLonger = let ps = [ Point (0, -3) Bottom
                           , Point (0,-2) Top
                           , Point (1,-3) Bottom
                           , Point (1,-2) Top
                           , Point (2,-3) Bottom
                           , Point (2,-2) Top]
                      es = map dupleToEdge . mapMaybe listToDuple $ windowed 2 ps
                  in mkRoadMap es Blue
validEdges :: Set UndirectedEdge
validEdges = Set.fromList [L.mkEdge (-1,-1,Top) (0,-3, Bottom), L.mkEdge (1,-3,Bottom) (0,-1,Top)]

invalidEdges :: Set UndirectedEdge
invalidEdges = Set.fromList [ L.mkEdge (2,-3,Bottom) (1,-1,Top)
                            , L.mkEdge (1,-2,Top) (2,-3,Bottom)
                            , L.mkEdge (2,-2, Top) (3,-3, Bottom)]

addedEdge :: UndirectedEdge
addedEdge = L.mkEdge (2,-3, Bottom) (2,-2, Top)

blueRoads2 :: RoadMap
blueRoads2 = under (_Unwrapping' RoadMap) (Map.union (unRoadMap $ mkRoadMap [addedEdge] Blue)) blueRoads

longerEdges :: [UndirectedEdge]
longerEdges = let ps = [ Point (0, -3) Bottom
                       , Point (-1,-1) Top
                       , Point (1,-2) Bottom
                       , Point (-2,0) Top
                       , Point (-2, -1) Bottom
                       , Point (-3,-1) Top
                       , Point (-2,0) Bottom]
              in map dupleToEdge . mapMaybe listToDuple $ windowed 2 ps

whiteRoads :: RoadMap
whiteRoads = mkRoadMap longerEdges White

orangeRoads :: RoadMap
orangeRoads = let es = [ L.mkEdge (-2,2,Top) (-1,0,Bottom)
                       , L.mkEdge (0,-2,Bottom) (0,-1,Top)
                       , L.mkEdge (-2,1,Top) (-1,0,Bottom)
                       , L.mkEdge (-1,-1,Top) (0,-2,Bottom)
                       , L.mkEdge (0,-1,Top) (1,-3,Bottom)
                       , L.mkEdge (-2,1,Top) (-1,-1,Bottom)
                       , L.mkEdge (-1,0,Bottom) (-1,1,Top)
                       , L.mkEdge (-1,1,Top) (0,-1,Bottom)
                       , L.mkEdge (-1,0,Top) (0,-1,Bottom)
                       , L.mkEdge (1,-3,Bottom) (1,-2,Top)
                       , L.mkEdge (-1,-1,Bottom) (-1,0,Top)
                       , L.mkEdge (-1,0,Top) (0,-2,Bottom)
                       , L.mkEdge (-1,-1, Top) (0, -3, Bottom)
                       , L.mkEdge (-2,0,Bottom) (-2,1,Top)
                       , L.mkEdge (-2,2,Top) (-1, 1, Bottom)]
             in mkRoadMap es Orange

functionsSpec :: Spec
functionsSpec = parallel $ do
  describe "build" $
    it "should return a changed board when building a valid item" $ property $
      \board construct -> validConstruct (construct :: Construct) (board :: Board) ==>
        board /= build construct board
  describe "validRoadsFor" $ do
    it "should never return more than 72 options" $ property $
      \board c -> let vr = validRoadsFor (c :: Color) board
                  in Set.size vr <= 72
    it "should never include an existing road among valid options" $ property $
      \board c -> let rm' = Map.keysSet $ getRoads (board :: Board)
                      vr = validRoadsFor (c :: Color) board
                      vr' = Set.map (^?! onEdge.edge) vr
                  in  (rm' `Set.intersection` vr') `shouldSatisfy` Set.null
    context "when an enemy building is on one of the endpoints" $ do
      let isEmpty' = Map.null . Map.mapMaybe id
      it "should prohibit building if player only controls one side" $ property $
        \rt -> let b = fromRTBoard rt
                   hasNeeded = isNothing (join $ Map.lookup addedEdge (unRoadMap $ _roads b)) && not (isEmpty' (unBuildingMap $ _buildings b))
               in hasNeeded ==> Set.map getE (validRoadsFor Blue b) === validEdges
      it "should allow building if player controls both sides" $ property $
        \rt -> let b = fromRTBoard rt
                   hasNeeded = isJust (join $ Map.lookup addedEdge (unRoadMap $ _roads b)) && not (isEmpty' (unBuildingMap $ _buildings b))
               in hasNeeded ==> Set.map getE (validRoadsFor Blue b) === Set.union invalidEdges validEdges
  describe "validSettlementsFor" $
    it "should never include an existing settlement among valid options" $ property $
      \board c -> let sm' = Map.keysSet $ Map.filter (\x -> x^.buildingType == Settlement) $ getHabitations (board :: Board)
                      vs = validSettlementsFor (c :: Color) board
                      vs' = Set.map (^?! onPoint.point) vs
                  in  (sm' `Set.intersection` vs') `shouldSatisfy` Set.null
  describe "validCitiesFor" $
    it "should return all points that have settlements" $ property $ 
      \board c -> let sp' = Map.keysSet . Map.filter (isSettlement . Building . Edifice) $ getHabitationsFor c board
                      vcp = mapSetMaybe (^?onPoint.point) $ validCitiesFor c board
                  in sp' === vcp
  describe "roadGraphForColor" $ do
    let b' = evalRand newBoard (mkStdGen 1)
    it "should return a graph of connected roads for a color" $ do
      let roads' = RoadMap $ Map.union (unRoadMap blueRoads2) (unRoadMap $ _roads b')
      let board' = b' { _roads = roads' }
      let gr' = roadGraphForColor Blue board'
      labNodes gr' `shouldSatisfy` (== 4) . length
      labEdges gr' `shouldSatisfy` (== 4) . length -- 2 edges, both ways
    it "should not have a link between roads separated by an enemy settlement" $ do
      let interruption = under (_Unwrapping' BuildingMap) (Map.union (unBuildingMap $ interruptSettlement (Point (-1,-1) Bottom))) emptyBuildingMap
      let orangeLongest = b' { _roads = orangeRoads, _buildings = interruption }
      let gr' = roadGraphForColor Orange orangeLongest
      let e1 = L.mkEdge (-1,-1,Bottom) (-2,1,Top)
      let e2 = L.mkEdge (-1,0,Top) (-1,-1,Bottom)
      -- putStrLn . showDot $ fglToDot gr' 
      labNodes gr' `shouldSatisfy` (== 15) . length
      labEdges gr' `shouldSatisfy` (== 36) . length
      let wrongEdge = find (\(_,_,(s1,s2)) -> let ss = [s1,s2] in e1 `elem` ss && e2 `elem` ss) (labEdges gr')
      wrongEdge `shouldSatisfy` isNothing
  describe "longestRoad" $ do
    let board' = evalRand newBoard (mkStdGen 0)
    let roads' = _roads board'
    it "should return the color of the player with the longest road and its length" $ do
      let blueLongest = RoadMap $ Map.union (unRoadMap blueRoads) (unRoadMap $ roads')
      let blueLongestBoard = board' { _roads = blueLongest }
      longestRoad blueLongestBoard `shouldBe` (Blue, 3)
    it "should return a different longest road when a longer road appears" $ do
      let redRoads = mkRoadMap longerEdges Red
      let redLongest = RoadMap $ Map.unions (map unRoadMap [redRoads, blueRoads, roads'])
      let redLongestBoard = board' { _roads = redLongest }
      longestRoad redLongestBoard `shouldBe` (Red, 6)
    it "should count only the longest path" $ do
      let interruption = under (_Unwrapping' BuildingMap) (Map.union (unBuildingMap $ interruptSettlement (Point (-1,-1) Bottom))) emptyBuildingMap
          orangeLongest = board' { _roads = orangeRoads, _buildings = interruption }
      longestRoad orangeLongest `shouldBe` (Orange, 9)
    it "should not count roads interrupted by enemy settlements" $ do
      let whiteLongestRoads = under (_Unwrapping' RoadMap) (Map.union (unRoadMap whiteRoads)) roads'
      let whiteLongest = board' { _roads = whiteLongestRoads }
      longestRoad whiteLongest `shouldBe` (White, 6)
      let p = Point (-2,0) Top
      let interruption = under (_Unwrapping' BuildingMap) (Map.union (unBuildingMap $ interruptSettlement p)) emptyBuildingMap
      let whiteLongestInterrupted = board' { _roads = whiteLongestRoads, _buildings = interruption }
      longestRoad whiteLongestInterrupted `shouldBe` (White, 3)
