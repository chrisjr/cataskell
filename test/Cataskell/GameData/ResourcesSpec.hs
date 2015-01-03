module Cataskell.GameData.ResourcesSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Data.Monoid
import Control.Applicative ((<$>))
import Cataskell.GameData.Basics
import Cataskell.GameData.Resources

instance Arbitrary ResourceCount where
  arbitrary = do
    NonNegative lumber' <- arbitrary
    NonNegative wool' <- arbitrary
    NonNegative wheat' <- arbitrary
    NonNegative brick' <- arbitrary
    NonNegative ore' <- arbitrary
    return $ ResourceCount lumber' wool' wheat' brick' ore'

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "A ResourceCount" $ do
    it "should start empty" $ do
      totalResources mempty `shouldBe` (0 :: Int)
    it "can be multiplied by a scalar" $ property $
      \x i -> totalResources (mulResources (x :: ResourceCount) (i :: Int)) == i * totalResources x
    it "should be invertible" $ property $
      \x -> (x <> mkNeg x) == (mempty :: ResourceCount)
    it "can be checked against costs (ex. 1)" $ do
      let res1 = ResourceCount { lumber = 2
                               , wool = 0
                               , wheat = 3
                               , brick = 3
                               , ore = 3
                               }
      (sufficient res1 . cost $ unbuilt settlement) `shouldBe` False
      (sufficient res1 . cost $ unbuilt city) `shouldBe` True
      (sufficient res1 . cost $ unbuilt road) `shouldBe` True
      (sufficient res1 . cost $ unbuilt devCard) `shouldBe` False
    it "can be checked against costs (ex. 2)" $ do
      let res2 = ResourceCount { lumber = 0
                               , wool = 2
                               , wheat = 4
                               , brick = 1
                               , ore = 2
                               }
      (sufficient res2 . cost $ unbuilt settlement) `shouldBe` False
      (sufficient res2 . cost $ unbuilt city) `shouldBe` False
      (sufficient res2 . cost $ unbuilt road) `shouldBe` False
      (sufficient res2 . cost $ unbuilt devCard) `shouldBe` True
    it "can be used to pay for an item" $ do
      let res3 = mempty { lumber = 1, brick = 1 }
      (payFor res3 $ unbuilt road) `shouldBe` Just mempty
      (payFor mempty $ unbuilt road) `shouldBe` Nothing
      let res4 = mempty { wool = 1, wheat = 1, ore = 1}
      (payFor res4 $ unbuilt devCard) `shouldBe` Just mempty
      (payFor mempty $ unbuilt devCard) `shouldBe` Nothing
    it "can be checked to be all non-negative" $ property $
      \res -> nonNegative (res :: ResourceCount)
  describe "resourceFromTerrain" $ do
    it "gets the appropriate resource for a terrain type" $ do
      resourceFromTerrain Forest `shouldBe` mempty { lumber =  1 }
      resourceFromTerrain Hill `shouldBe` mempty { brick =  1 }
      resourceFromTerrain Pasture `shouldBe` mempty { wool =  1 }
      resourceFromTerrain Mountain `shouldBe` mempty { ore =  1 }
      resourceFromTerrain Desert `shouldBe` mempty
      resourceFromTerrain Field `shouldBe` mempty { wheat = 1 }
      
