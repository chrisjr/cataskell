module Cataskell.GameData.PlayerSpec (main, spec) where

import Test.Hspec
import Cataskell.GameData.Player
import Cataskell.GameData.Resources

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "A Player" $ do
    let p = Player { }
    it "begins with 0 resources" $ do
      (totalResources $ resources p) `shouldBe` 0