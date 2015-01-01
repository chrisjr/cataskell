module Cataskell.GameData.ActionsSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Cataskell.GameData.Actions
import Cataskell.GameData.Player

instance Arbitrary PlayerAction where
  arbitrary = undefined

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "A PlayerAction" $ do
    it "should have an associated player" $ property $
      \pAction -> validPlayer $ player (pAction :: PlayerAction)
