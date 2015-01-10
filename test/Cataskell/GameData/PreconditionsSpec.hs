module Cataskell.GameData.PreconditionsSpec where

import Test.Hspec
import Cataskell.GameData.Preconditions
import Control.Exception (evaluate)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  let pred' i = (i :: Int) == 0
  let p = Precondition { predicate = pred', label = "isZero" }
  
  let pred2 i = (i :: Int) < 1
  let p2 = Precondition { predicate = pred2, label = "lessThan1" }
  
  let pred3 i = (i :: Int) > -2
  let p3 = Precondition { predicate = pred3, label = "greaterThanNeg2"}

  describe "A Precondition" $ do
   it "has a predicate and a label" $ do
     let p' = predicate p
     p' 0 `shouldBe` True
     label p `shouldBe` "isZero"
  describe "check" $ do
    it "should return Right True if predicate is satisfied, and Left String otherwise" $ do
      check p 0 `shouldBe` Right True
      check p 1 `shouldBe` Left "isZero"
  describe "checkUnsafe" $ do
    it "should throw an error using the precondition's label" $ do
      evaluate (checkUnsafe p 1) `shouldThrow` errorCall (label p)
  describe "checkAll" $ do
    it "should check predicates and return Right True or a Left String (concatenated) of failures" $ do
      checkAll [p, p2, p3] 0 `shouldBe` Right True
      checkAll [p, p2, p3] (-1) `shouldBe` Left [label p]
      checkAll [p, p2, p3] (-2) `shouldBe` Left [label p, label p3]
      checkAll [p, p2, p3] 1 `shouldBe` Left [label p, label p2]
  describe "checkAllUnsafe" $ do
    it "should check predicates and throw errors if they fail" $ do
      checkAllUnsafe [p, p2, p3] 0 `shouldBe` True
      evaluate (checkAllUnsafe [p, p2, p3] (-1)) `shouldThrow` errorCall (unlines [label p])
      evaluate (checkAllUnsafe [p, p2, p3] (-2)) `shouldThrow` errorCall (unlines [label p, label p3])
      evaluate (checkAllUnsafe [p, p2, p3] 1) `shouldThrow` errorCall (unlines [label p, label p2])
