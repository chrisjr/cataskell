module Cataskell.GameData.ActionsSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck hiding (reason)

import Cataskell.GameData.Actions
import Cataskell.GameData.Basics
import Cataskell.GameData.Player
import Cataskell.GameData.Resources

import Data.Monoid (mempty)
import Data.Maybe
import Control.Lens

import Cataskell.GameData.PlayerSpec()
import Cataskell.GameData.ResourcesSpec()

instance Arbitrary DiscardAction where
  arbitrary = do
    r <- arbitrary
    let t = totalResources r
    return $ DiscardAction { _resourcesDiscarding = r, _amountToDiscard = t}

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "A PlayerAction" $ do
    let pAction = PlayerAction { _actor = mkPlayer (Blue, "NoOne"), _action = Roll }
    it "should have an associated actor" $ do
      pAction ^. actor `shouldSatisfy` validPlayer
    it "should have an action" $ do
      pAction ^. action `shouldBe` Roll
  describe "A TradeAction" $ do
    let oneWheat = (mempty { wheat = 1 }) :: ResourceCount
    let oneOre = (mempty { ore = 1 }) :: ResourceCount
    let offer' = TradeOffer { _offering = oneWheat, _asking = oneOre }
    let p1 = (mkPlayer (Blue, "NoOne")) { resources = mempty { wheat = 2, ore = 2 } }
    let p1offer = mkOffer offer' p1
    let p2 = (mkPlayer (White, "Nobody")) { resources = mempty { ore = 3 } }
    let p3 = mkPlayer (Orange, "Nadie")

    describe "A TradeOffer" $ do
      it "should have an offering and asking amount" $ do
        view offering offer' `shouldBe` oneWheat
        view asking offer' `shouldBe` oneOre
      it "can be checked against the resources of the player" $ do
        p1 `shouldSatisfy` (\p -> enoughFor (p1offer ^? action.trade.offer.offering) p == (Just True))

    let p2accept = fromJust $ accept p1offer p2
    describe "An Accept" $ do
      it "should contain the accepter, the original offer and the asker" $ do
        view actor p2accept `shouldBe` p2
        p2accept ^? action.trade.offer `shouldBe` Just offer'
        p2accept ^? action.trade.asker `shouldBe` Just p1
      it "can be checked against the resources of the player" $ do
        p2 `shouldSatisfy` (\p -> enoughFor (p2accept ^? action.trade.offer.asking) p == (Just True))

    let p3reject = fromJust $ reject p1offer p3 (Just "nope")
    describe "A Reject" $ do
      it "should contain the rejecter, original offer and asker" $ do
        view actor p3reject `shouldBe` p3
        p3reject ^? action.trade.offer `shouldBe` Just offer'
        p3reject ^? action.trade.asker `shouldBe` Just p1
      it "may contain a reason for rejection" $ do
        p3reject ^? action.trade.reason `shouldBe` (Just (Just "nope"))

    let p1complete = fromJust $ complete p1offer p2accept
    describe "A CompleteTrade" $ do
      it "can be checked against the resources of both players" $ do
        p1 `shouldSatisfy` (\p -> enoughFor (p1complete ^? action.trade.offer.offering) p == (Just True))
        p2 `shouldSatisfy` (\p -> enoughFor (p1complete ^? action.trade.offer.asking) p == (Just True))
    describe "A CancelTrade" $ do
      it "should clear an extant trade offer from this player" $ do
        pendingWith "need GameState and update functions to test"
  describe "A Discard action" $ do
    it "should have a necessary amount to discard" $ do
      let dAction = DiscardAction { _amountToDiscard = 4, _resourcesDiscarding = mempty}
      view amountToDiscard dAction `shouldBe` 4
    it "should contain a set of resources that equal the discard amount" $ property $
      \x -> view amountToDiscard (x :: DiscardAction) == (totalResources $ view resourcesDiscarding x)
  describe "rollFor" $ do
    it "produces a roll action for a player" $ property $
      \p -> view action (rollFor (p :: Player)) == Roll

