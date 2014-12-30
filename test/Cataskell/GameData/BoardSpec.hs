module Cataskell.GameData.BoardSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Cataskell.GameData.Board
import Cataskell.GameData.Basics
import Cataskell.GameData.Resources
import Data.List
import qualified Data.Map.Strict as Map
import Data.Monoid (mempty)
import Control.Monad.Random

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "A Hex" $ do
    it "has a terrain type and accompanying resource" $ do
      let hex = mkHex Mountain 2
      resource hex `shouldBe` mempty { ore = 1 }

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
      (all (== 2) $ map ((Map.!) rollCounts) [3..11]) `shouldBe` True

  describe "A Board" $ do
    let rand = mkStdGen 999
    let (board, g') = runRand newBoard rand
    let h = hexes board
    it "has a randomly generated set of terrains" $ do
      Map.size h `shouldBe` 19
    it "has no high-value terrains (6 or 8) next to each other" $ do
      Map.size h `shouldBe` 19
      True `shouldBe` False
