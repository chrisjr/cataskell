module Cataskell.GameSpec (main, spec) where

import Test.Hspec
import Cataskell.Game
import Cataskell.GameData
import Cataskell.GameData.Basics
import Cataskell.GameData.Board
import System.Random

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "A new game" $ do
    let rand = mkStdGen 0
    let (g, s) = newGame rand
    it "should start off with no placements" $ do
      let b = board g
      placements b `shouldBe` []
