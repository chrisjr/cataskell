module Cataskell.GameData.ActionsSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck hiding (reason)

import Cataskell.GameData.Actions
import Cataskell.GameData.Basics
import Cataskell.GameData.Player
import Cataskell.GameData.Resources

import Data.Monoid (mempty)
import Data.Maybe

import Cataskell.GameData.PlayerSpec()
import Cataskell.GameData.ResourcesSpec()

instance Arbitrary DiscardAction where
  arbitrary = do
    r <- arbitrary
    let t = totalResources r
    return $ DiscardAction { resourcesDiscarding = r, amountToDiscard = t}

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "A PlayerAction" $ do
    let pAction = PlayerAction { actor = mkPlayer (Blue, "NoOne"), action = Roll }
    it "should have an associated actor" $ do
      actor pAction `shouldSatisfy` validPlayer
    it "should have an action" $ do
      action pAction `shouldBe` Roll
  describe "A TradeAction" $ do
    let oneWheat = (mempty { wheat = 1 }) :: ResourceCount
    let oneOre = (mempty { ore = 1 }) :: ResourceCount
    let offer' = TradeOffer { offering = oneWheat, asking = oneOre }
    let p1 = (mkPlayer (Blue, "NoOne")) { resources = mempty { wheat = 2, ore = 2 } }
    let p1offer = mkOffer offer' p1
    let p2 = (mkPlayer (White, "Nobody")) { resources = mempty { ore = 3 } }
    let p3 = mkPlayer (Orange, "Nadie")

    describe "A TradeOffer" $ do
      it "should have an offering and asking amount" $ do
        offering offer' `shouldBe` oneWheat
        asking offer' `shouldBe` oneOre
      it "can be checked against the resources of the player" $ do
        p1 `shouldSatisfy` (\p -> enoughFor p1offer offering p)

    let p2accept = accept p1offer p2
    describe "An Accept" $ do
      it "should contain the accepter, the original offer and the asker" $ do
        actor p2accept `shouldBe` p2
        let tr = getTrade p2accept
        offer tr `shouldBe` offer'
        asker tr `shouldBe` p1
      it "can be checked against the resources of the player" $ do
        p2 `shouldSatisfy` (\p -> enoughFor p2accept asking p)

    let p3reject = reject p1offer p3 (Just "nope")
    describe "A Reject" $ do
      it "should contain the rejecter, original offer and asker" $ do
        actor p3reject `shouldBe` p3
        let tr = getTrade p3reject
        offer tr `shouldBe` offer'
        asker tr `shouldBe` p1
      it "may contain a reason for rejection" $ do
        reason (getTrade p3reject) `shouldBe` (Just "nope")

    let p1complete = complete p1offer p2accept
    describe "A CompleteTrade" $ do
      it "can be checked against the resources of both players" $ do
        p1 `shouldSatisfy` (\p -> enoughFor p1complete offering p)
        p2 `shouldSatisfy` (\p -> enoughFor p1complete asking p)
    describe "A CancelTrade" $ do
      it "should clear an extant trade offer from this player" $ do
        pending
  describe "A Discard action" $ do
    it "should have a necessary amount to discard" $ do
      let dAction = DiscardAction { amountToDiscard = 4, resourcesDiscarding = mempty}
      amountToDiscard dAction `shouldBe` 4
    it "should contain a set of resources that equal the discard amount" $ property $
      \x -> amountToDiscard (x :: DiscardAction) == (totalResources $ resourcesDiscarding x)
  describe "rollFor" $ do
    it "produces a roll action for a player" $ property $
      \p -> action (rollFor (p :: Player)) == Roll

