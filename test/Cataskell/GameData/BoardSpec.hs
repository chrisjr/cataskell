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

instance NFData Terrain
instance NFData Hex

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "A Hex" $ do
    it "has a terrain type and accompanying resource" $ do
      let hex = mkHex Mountain 2
      resource hex `shouldBe` mempty { ore = 1 }
    it "cannot be made with Desert && roll != 7, or !Desert && roll == 7" $ do
      terrain (mkHex Desert 7) `shouldBe` Desert
      (evaluate . force) (mkHex Desert 5) `shouldThrow` (const True :: Selector AssertionFailed)
      (evaluate . force) (mkHex Mountain 7) `shouldThrow` (const True :: Selector AssertionFailed)

  describe "A HexGenerator" $ do
    let rand = mkStdGen 0
    let hexMap = evalRand newHexMap rand

    it "should create a randomly generated set of terrains and rolls" $ do
      (Map.size hexMap) `shouldBe` 19

    it "should have one 2, one 7, one 12, and two of everything else" $ do
      let rolls = group . sort . map snd . Map.toList $ Map.map roll hexMap
      let rollCounts = Map.fromList $ zip (map head rolls) (map length rolls)
      ((Map.!) rollCounts 2) `shouldBe` 1
      ((Map.!) rollCounts 7) `shouldBe` 1
      ((Map.!) rollCounts 12) `shouldBe` 1
      (all (== 2) $ map ((Map.!) rollCounts) [3..6]) `shouldBe` True
      (all (== 2) $ map ((Map.!) rollCounts) [8..11]) `shouldBe` True

    it "should have 3 hills, 4 pastures, 3 mountains, 4 fields, 4 forests, and 1 desert" $ do
      let terrains = group . sort . map snd . Map.toList $ Map.map terrain hexMap
      let terrainCounts = Map.fromList $ zip (map head terrains) (map length terrains)
      ((Map.!) terrainCounts Hill) `shouldBe` 3
      ((Map.!) terrainCounts Pasture) `shouldBe` 4
      ((Map.!) terrainCounts Mountain) `shouldBe` 3
      ((Map.!) terrainCounts Field) `shouldBe` 4
      ((Map.!) terrainCounts Forest) `shouldBe` 4
      ((Map.!) terrainCounts Desert) `shouldBe` 1

    describe "the Desert" $ do
      it "should have roll equal to 7" $ do
        let desertHex = head . filter ((== Desert) . terrain) . map snd $ Map.toList hexMap
        roll desertHex `shouldBe` 7

  describe "A Board" $ do
    let rand = mkStdGen 999
    let (board, g') = runRand newBoard rand
    let h = hexes board
    describe "has a HexMap" $ do
      it "with a randomly generated set of terrains" $ do
        Map.size h `shouldBe` 19
      it "with no high-value terrains (6 or 8) next to each other" $ do
        Map.size h `shouldBe` 19
        checkNeighbors h `shouldBe` True
