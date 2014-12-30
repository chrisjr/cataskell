module Cataskell.GameData.BoardSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Cataskell.GameData.Board
import Cataskell.GameData.Basics
import Cataskell.GameData.Resources
import Data.Monoid (mempty)
import System.Random

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "A Hex" $ do
    it "has a terrain type and accompanying resource" $ do
      let hex = mkHexGraph Mountain 2
      resource hex `shouldBe` mempty { ore = 1 }
  describe "A Board" $ do
    let rand = mkStdGen 0
    let (board, g') = newBoard rand
    let h = hexes board
    it "has a randomly generated set of terrains" $ do
      length h `shouldBe` 19
    it "has no high-value terrains (6 or 8) next to each other" $ do
      length h `shouldBe` 19
      True `shouldBe` False
