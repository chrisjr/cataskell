module Cataskell.GameData.ResourcesSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Data.Monoid
import qualified Data.Set as Set
import Data.List (maximumBy)
import Data.Ord (comparing)
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

instance Arbitrary ResourceType where
  arbitrary = elements [Lumber, Wool, Wheat, Brick, Ore]

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
  describe "filteredResCounts" $ do
    it "should get a new resource count containing only resources of a certain type" $ do
      let res = ResourceCount { lumber = 1
                              , wool = 2
                              , wheat = 3
                              , brick = 4
                              , ore = 5
                              }
      filteredResCount Lumber res `shouldBe` mempty { lumber =  1 }
      filteredResCount Wool res `shouldBe` mempty { wool =  2 }
      filteredResCount Wheat res `shouldBe` mempty { wheat = 3}
      filteredResCount Brick res `shouldBe` mempty { brick =  4 }
      filteredResCount Ore res `shouldBe` mempty { ore =  5 }
  describe "nResOf" $
    it "should produce n resources of the specified type" $ property $
      \i resType -> totalResources (nResOf i resType) == i
  describe "Harbors" $ do
    let woolH = HarborDiscount mempty { wool = 2}
    let wheatH = HarborDiscount mempty { wheat = 2}
    let brickH = HarborDiscount mempty { brick = 2}
    let oreH = HarborDiscount mempty { ore = 2}
    let lumberH = HarborDiscount mempty { lumber = 2}
    let threeToOneH = Set.fromList [ HarborDiscount mempty { wool = 3}
                                   , HarborDiscount mempty { wheat = 3}
                                   , HarborDiscount mempty { lumber = 3}
                                   , HarborDiscount mempty { brick = 3}
                                   , HarborDiscount mempty { ore = 3}]

    describe "harborDiscount" $
      it "should return a Set of HarborDiscounts when given a harbor" $ do
        harborDiscount (Harbor Hill) `shouldBe` Set.singleton brickH
        harborDiscount (Harbor Pasture) `shouldBe` Set.singleton woolH
        harborDiscount (Harbor Field) `shouldBe` Set.singleton wheatH
        harborDiscount (Harbor Mountain) `shouldBe` Set.singleton oreH
        harborDiscount (Harbor Forest) `shouldBe` Set.singleton lumberH
        harborDiscount ThreeToOne `shouldBe` threeToOneH
    describe "applyDiscount" $ do
      it "should return an amount generated and a resource remainder from offered resources" $ do
        let res = mempty { wool = 6 }
        let (n, res') = applyDiscount res woolH
        n `shouldBe` 3
        res' `shouldBe` mempty
        let (n', res'') = applyDiscount res oreH
        n' `shouldBe` 0
        res'' `shouldBe` res
        let (n'', res''') = maximumBy (comparing fst) $ map (applyDiscount res) (Set.toList threeToOneH)
        n'' `shouldBe` 2
        res''' `shouldBe` mempty
