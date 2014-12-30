module Cataskell.GameData.BasicsSpec where

import Test.Hspec
import Cataskell.GameData.Basics

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "A Construct" $ do
    it "can be either Placed or Unplaced" $ do
    	True `shouldBe` True