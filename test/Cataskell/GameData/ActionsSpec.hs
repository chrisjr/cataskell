module Cataskell.GameData.ActionsSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Cataskell.GameData.Actions
import Cataskell.GameData.Player
import Cataskell.GameData.Resources

instance Arbitrary PlayerAction where
  arbitrary = undefined

instance Arbitrary DiscardAction where
  arbitrary = undefined

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "A PlayerAction" $ do
    it "should have an associated actor" $ do
      pending
    it "should have an action" $ do
      pending
  describe "A TradeAction" $ do
    describe "A TradeOffer" $ do
      it "should have an offering and asking amount" $ do
        pending
      it "should be checked against the resources of the player" $ do
        pending
    describe "An Accept" $ do
      it "should contain the original offer and asker" $ do
        pending
      it "should be checked against the resources of the player" $ do
        pending
    describe "A Reject" $ do
      it "should contain the original offer and asker" $ do
        pending
      it "may contain a reason for rejection" $ do
        pending
    describe "A CompleteTrade" $ do
      it "should be checked against the resources of both players" $ do
        pending
      it "should execute the change of resources" $ do
        pending
    describe "A CancelTrade" $ do
      it "should clear all extant trade offers from this player" $ do
        pending
  describe "A Discard action" $ do
    it "should have a necessary amount to discard" $ do
      pending
    it "should contain a set of resources that equal the discard amount" $ property $
      \x -> amountToDiscard (x :: DiscardAction) == (totalResources $ resourcesDiscarding x)