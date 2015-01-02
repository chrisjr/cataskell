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
import Data.List
import qualified Data.Map.Strict as Map
import Data.Monoid (mempty)
import Control.Monad.Random
import Control.Applicative ((<$>))

import Cataskell.GameData.LocationSpec()

instance NFData Terrain
instance NFData HexCenter

instance Arbitrary HexCenter where
  arbitrary = do
    terrain' <- elements (Desert:terrains)
    roll' <- if terrain' == Desert then elements [7] else elements rolls
    return $ mkHexCenter terrain' roll' 

instance Arbitrary HexMap where
  arbitrary = do
    seed <- arbitrary
    return $ evalRand newHexMap $ mkStdGen seed
  shrink m = Map.fromList <$> shrink (Map.toList m)

instance Arbitrary Board where
  arbitrary = do
    seed <- arbitrary
    return $ evalRand newBoard $ mkStdGen seed

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "A HexCenter" $ do
    it "has a terrain type and accompanying resource" $ do
      let hex = mkHexCenter Mountain 2
      resource hex `shouldBe` mempty { ore = 1 }
    it "cannot be made with Desert && roll != 7, or !Desert && roll == 7" $ do
      terrain (mkHexCenter Desert 7) `shouldBe` Desert
      (evaluate . force) (mkHexCenter Desert 5) `shouldThrow` (const True :: Selector AssertionFailed)
      (evaluate . force) (mkHexCenter Mountain 7) `shouldThrow` (const True :: Selector AssertionFailed)
    it "can be generated satisfying this requirement" $ property $
      \hc -> (terrain (hc :: HexCenter) == Desert) == (roll hc == 7)
  describe "A HexMap" $ do
    it "should create a randomly generated set of terrains and rolls" $ property $
      \hexMap -> (Map.size (hexMap :: HexMap)) == 19
    it "should have one 2, one 7, one 12, and two of everything else" $ property $
      \hexMap -> let rolls = group . sort . map roll $ Map.elems (hexMap :: HexMap)
                     rollCounts = Map.fromList $ zip (map head rolls) (map length rolls)
                     a = ((Map.!) rollCounts 2) == 1
                     b = ((Map.!) rollCounts 7) == 1
                     c = ((Map.!) rollCounts 12) == 1
                     d = (all (== 2) $ map ((Map.!) rollCounts) [3..6])
                     e = (all (== 2) $ map ((Map.!) rollCounts) [8..11])
                 in  a && b && c && d && e

    it "should have 3 hills, 4 pastures, 3 mountains, 4 fields, 4 forests, and 1 desert" $ property $
      \hexMap -> let terrains = group . sort . map terrain $ Map.elems (hexMap :: HexMap)
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
        \hexMap -> let desertHex = head . filter ((== Desert) . terrain) $ Map.elems (hexMap :: HexMap)
                   in roll desertHex == 7
  describe "A BuildingMap" $ do
    it "should start off empty" $ do
      let e = emptyBuildingMap
      True `shouldBe` True
      -- all () $ map Map. `shouldBe`

  describe "A Board" $ do
    it "should start off with no buildings and no roads" $ property $
      \board -> buildings (board :: Board) == emptyBuildingMap
