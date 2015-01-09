{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Cataskell.GameData.BoardSpec (main, spec) where

import Test.Hspec
import Control.Exception
import Control.DeepSeq
import Test.QuickCheck
import Cataskell.GameData.Board
import Cataskell.GameData.Basics
import Cataskell.GameData.Resources
import Cataskell.GameData.Location
import Data.List
import qualified Data.Map.Strict as Map
import Data.Monoid (mempty)
import Data.Maybe
import Control.Lens hiding (elements)
import Control.Monad.Random
import Control.Applicative ((<$>), (<*>))
import Control.Arrow ((&&&))

import Cataskell.GameData.LocationSpec()
import Cataskell.GameData.BasicsSpec ()
import Cataskell.UtilSpec() -- for Arbitrary StdGen instance

instance NFData Terrain
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
  shrink m = Map.fromList <$> shrink (Map.toList m)

instance Arbitrary OnEdge where
  arbitrary = OnEdge <$> arbitrary <*> arbitrary

instance Arbitrary OnPoint where
  arbitrary = OnPoint <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary RoadMap where
  arbitrary = do
    roads' <- listOf (arbitrary :: Gen OnEdge)
    let withRoads = Map.fromList $ map (\r -> (r^.edge, Just r)) roads'
    return $ Map.union withRoads emptyRoadMap
  shrink m = Map.fromList <$> shrink (Map.toList m)

instance Arbitrary BuildingMap where
  arbitrary = do
    buildings' <- listOf (arbitrary :: Gen OnPoint)
    let withBuildings = Map.fromList $ map (\b -> (b^.point, Just b)) buildings'
    return $ Map.union withBuildings emptyBuildingMap
  shrink m = Map.fromList <$> shrink (Map.toList m)

instance Arbitrary Harbor where
  arbitrary = elements harborTypes

instance Arbitrary HarborMap where
  arbitrary = do
    stdGen <- (arbitrary :: Gen StdGen)
    return $ evalRand newHarborMap stdGen
  shrink m = Map.fromList <$> shrink (Map.toList m)

instance Arbitrary Board where
  arbitrary = do
    stdGen <- (arbitrary :: Gen StdGen)
    return $ evalRand newBoard stdGen
  shrink b = tail $ Board <$> shrink' (_hexes b) 
                          <*> shrink' (_roads b)
                          <*> shrink' (_buildings b)
                          <*> shrink' (_harbors b)
    where shrink' a = a : shrink a

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
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
  describe "A HexMap" $ do
    it "should create a randomly generated set of terrains and rolls" $ property $
      \hexMap -> (Map.size (hexMap :: HexMap)) == 19
    it "should have one 2, one 7, one 12, and two of everything else" $ property $
      \hexMap -> let rolls' = group . sort . map _roll $ Map.elems (hexMap :: HexMap)
                     rollCounts = Map.fromList $ map (head &&& length) rolls'
                     a = ((Map.!) rollCounts 2) == 1
                     b = ((Map.!) rollCounts 7) == 1
                     c = ((Map.!) rollCounts 12) == 1
                     d = (all (== 2) $ map ((Map.!) rollCounts) [3..6])
                     e = (all (== 2) $ map ((Map.!) rollCounts) [8..11])
                 in  a && b && c && d && e
    it "should have 3 hills, 4 pastures, 3 mountains, 4 fields, 4 forests, and 1 desert" $ property $
      \hexMap -> let terrains = group . sort . map _terrain $ Map.elems (hexMap :: HexMap)
                     terrainCounts = Map.fromList $ zip (map head terrains) (map length terrains)
                     h = ((Map.!) terrainCounts Hill) == 3
                     p = ((Map.!) terrainCounts Pasture) == 4
                     m = ((Map.!) terrainCounts Mountain) == 3
                     fi = ((Map.!) terrainCounts Field) == 4
                     fo = ((Map.!) terrainCounts Forest) == 4
                     d = ((Map.!) terrainCounts Desert) == 1
                 in h && p && m && fi && fo && d

    it "should have no high-value terrains (6 or 8) next to each other" $ property $
      \hexMap -> checkHexNeighbors hexMap == True

    describe "the Desert" $ do
      it "should have roll equal to 7" $ property $
        \hexMap -> let desertHex = head . filter ((== Desert) . _terrain) $ Map.elems (hexMap :: HexMap)
                   in _roll desertHex == 7
  describe "A BuildingMap" $ do
    it "should have 54 keys" $ do
      Map.size emptyBuildingMap `shouldBe` 54
    it "should start off empty" $ do
      Map.elems emptyBuildingMap `shouldSatisfy` (all (== Nothing))
  describe "A RoadMap" $ do
    it "should have 72 keys" $ do
      Map.size emptyRoadMap `shouldBe` 72
    it "should start off empty" $ do
      Map.elems emptyRoadMap `shouldSatisfy` (all (== Nothing))
  describe "A Board" $ do
    it "should start off with no buildings and no roads" $ property $
      \board -> view buildings (board :: Board) == emptyBuildingMap
    it "can be queried for open points" $ property $
      \board -> length (freePoints (board :: Board)) == 54
    it "can be queried for open points after building" $ property $
      \p board -> let bldg = built . settlement $ Just ((p :: Point), Blue)
                      board' = build bldg (board :: Board)
                      l = length (freePoints board')
                  in  l == 51 || l == 50 -- at least 3 points now off limits, possibly 4
    it "can be queried for colors affected by a roll" $ do
      let board = evalRand newBoard $ mkStdGen 0
      -- print $ Map.filter ((== 6) . roll) (board ^. hexes)
      let bldg = built . settlement $ Just ((Point (0,0) Top), Blue)
      let board' = build bldg board
      let res' = allResourcesFromRoll 6 board'
      res' `shouldBe` Map.singleton Blue mempty { ore = 1 }
  describe "build" $ do
    it "should return a changed board when building a valid item" $ property $
      \board construct -> validConstruct (construct :: Construct) (board :: Board) ==>
        board /= build construct board
  describe "validRoadsFor" $ do
    it "should never include an existing road among valid options" $ property $
      \board c -> let rm' = Map.keys $ getRoads (board :: Board)
                      vr = validRoadsFor (c :: Color) board
                      vr' = map (^?! onEdge.edge) vr
                  in  not $ any (`elem` rm') vr'
  describe "validSettlementsFor" $ do
    it "should never include an existing settlement among valid options" $ property $
      \board c -> let sm' = Map.keys $ Map.filter (\x -> x^.buildingType == Settlement) $ getHabitations (board :: Board)
                      vs = validSettlementsFor (c :: Color) board
                      vs' = map (^?! onPoint.point) vs
                  in  not $ any (`elem` sm') vs'
