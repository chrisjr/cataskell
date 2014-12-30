module Cataskell.GameData.PlayerSpec (main, spec) where

import Test.Hspec
import Data.Monoid
import Cataskell.GameData.Basics
import Cataskell.GameData.Player
import Cataskell.GameData.Resources

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "A Player" $ do
    let p = player Blue
    it "should begin with 0 resources" $ do
      (totalResources $ resources p) `shouldBe` 0
    it "can add resources" $ do
      let p' = p { resources = mempty { ore = 1 }}
      (totalResources $ resources p') `shouldBe` 1

    let p2 = (player White) {
        constructed = [ DevCard (Just VictoryPoint)
                      , Building (Habitation Settlement (Just undefined))
                      , Building (Habitation Settlement (Just undefined))]
      }

    it "should have a score" $ do
      getScore p2 `shouldBe` 3
    it "should have a display score" $ do
      getDisplayScore p2 `shouldBe` 2
