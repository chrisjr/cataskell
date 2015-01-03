module Cataskell.GameData.PlayerSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Data.Monoid
import Cataskell.GameData.Basics
import Cataskell.GameData.Player
import Cataskell.GameData.Resources
import Control.Lens hiding (elements)

import Cataskell.GameData.ResourcesSpec() -- get Arbitrary ResourceCount 

instance Arbitrary Player where
  arbitrary = do
    name <- elements ["1", "2", "3", "4"]
    color <- elements [Red, Blue, Orange, White]
    let p = mkPlayer (color, name)
    r <- arbitrary
    return $ resources .~ r $ p

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "A Player" $ do
    let p = mkPlayer (Blue, "Nobody")
    it "has a name" $ do
      view playerName p `shouldBe` "Nobody"
    it "should begin with 0 resources" $ do
      (totalResources $ view resources p) `shouldBe` 0
    it "can add resources" $ property $
      \p -> let resCountNow = totalResources $ view resources (p :: Player)
                oneOre = mempty { ore = 1 }
                resAfter = (view resources p) <> oneOre
                resCountAfter = totalResources resAfter
            in (resCountNow + 1) == resCountAfter
 
    let c' = [ Card VictoryPoint
             , Building (OnPoint (H Settlement undefined White))
             , Building (OnPoint (H Settlement undefined White))]
    let p2 = constructed .~ c' $ (mkPlayer (White, "No-One"))
    it "should have a score" $ do
      view score p2 `shouldBe` 3
    it "should have a display score" $ do
      view displayScore p2 `shouldBe` 2
    it "can have development cards" $ do
      view devCards p2 `shouldBe` [VictoryPoint]
    it "must have only non-negative resources" $ property $ do
      \player -> nonNegative $ view resources (player :: Player)