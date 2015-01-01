module Cataskell.GameData.PlayerSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Data.Monoid
import Cataskell.GameData.Basics
import Cataskell.GameData.Player
import Cataskell.GameData.Resources

import Cataskell.GameData.ResourcesSpec() -- get Arbitrary ResourceCount 

instance Arbitrary Player where
  arbitrary = undefined


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "A Player" $ do
    let p = mkPlayer Blue
    it "should begin with 0 resources" $ do
      (totalResources $ resources p) `shouldBe` 0
    it "can add resources" $ do
      let p' = p { resources = mempty { ore = 1 }}
      (totalResources $ resources p') `shouldBe` 1

    let p2 = (mkPlayer White) {
        constructed = [ Card VictoryPoint
                      , Building (OnPoint (H Settlement undefined White))
                      , Building (OnPoint (H Settlement undefined White))]
      }

    it "should have a score" $ do
      score p2 `shouldBe` 3
    it "should have a display score" $ do
      displayScore p2 `shouldBe` 2
    it "can have development cards" $ do
      devCards p2 `shouldBe` [VictoryPoint]
    it "must have only non-negative resources" $ property $ do
      \player -> nonNegative $ resources (player :: Player)