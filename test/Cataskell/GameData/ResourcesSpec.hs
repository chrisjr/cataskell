module Cataskell.GameData.ResourcesSpec where

import Test.Hspec
import Test.QuickCheck
import Data.Monoid
import Control.Applicative ((<$>))
import Cataskell.GameData.Basics
import Cataskell.GameData.Resources

instance Arbitrary ResourceCount where
  arbitrary = do
    lumber' <- arbitrary
    wool' <- arbitrary
    wheat' <- arbitrary
    brick' <- arbitrary
    ore' <- arbitrary
    return $ ResourceCount lumber' wool' wheat' brick' ore'

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "A ResourceCount" $ do
    it "should start empty" $ do
      totalResources mempty `shouldBe` (0 :: Int)
    it "should be invertible" $ property $
      \x -> (x <> mkNeg x) == (mempty :: ResourceCount)
    it "can be checked against costs (ex. 1)" $ do
      let res1 = ResourceCount { lumber = 2
                               , wool = 0
                               , wheat = 3
                               , brick = 3
                               , ore = 3
                               }
      (sufficient res1 $ cost Settlement) `shouldBe` False
      (sufficient res1 $ cost City) `shouldBe` True
      (sufficient res1 $ cost DevelopmentCard) `shouldBe` False
      (sufficient res1 $ cost Road) `shouldBe` True
      let res1' = res1 <> (mkNeg $ cost Road)
      (sufficient res1' $ cost Road) `shouldBe` True
      let res1'' = res1' <> (mkNeg $ cost Road)
      (sufficient res1'' $ cost Road) `shouldBe` False
    it "can be checked against costs (ex. 2)" $ do
      let res2 = ResourceCount { lumber = 0
                               , wool = 2
                               , wheat = 4
                               , brick = 1
                               , ore = 2
                               }
      (sufficient res2 $ cost Settlement) `shouldBe` False
      (sufficient res2 $ cost City) `shouldBe` False
      (sufficient res2 $ cost DevelopmentCard) `shouldBe` True
      (sufficient res2 $ cost Road) `shouldBe` False
      let res2' = res2 <> (mkNeg $ cost DevelopmentCard)
      (sufficient res2' $ cost DevelopmentCard) `shouldBe` True
      let res2'' = res2' <> mempty { lumber = 1 }
      (sufficient res2'' $ cost Settlement) `shouldBe` True
      
  describe "resourceFromTerrain" $ do
    it "gets the appropriate resource for a terrain type" $ do
      resourceFromTerrain Forest `shouldBe` mempty { lumber =  1 }
      resourceFromTerrain Hill `shouldBe` mempty { brick =  1 }
      resourceFromTerrain Pasture `shouldBe` mempty { wool =  1 }
      resourceFromTerrain Mountain `shouldBe` mempty { ore =  1 }
      resourceFromTerrain Desert `shouldBe` mempty
      resourceFromTerrain Field `shouldBe` mempty { wheat = 1 }
      
