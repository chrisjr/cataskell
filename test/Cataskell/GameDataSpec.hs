module Cataskell.GameDataSpec where

import Test.Hspec
import Test.QuickCheck
import Data.Monoid
import Control.Applicative ((<$>))
import Cataskell.GameData
import Cataskell.GameData.Basics
import Cataskell.GameData.Resources

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "A Hex" $ do
    it "has a terrain type and accompanying resource" $ do
      let hex = mkHex Mountain 2
      resource hex `shouldBe` mempty { ore = 1 }
      
