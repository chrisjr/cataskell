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
    let player' = mkPlayer (0, Blue, "NoOne")
    let players' = [player']
    let pAction = PlayerAction { _actor = player'^.playerIndex, _action = Roll }
    it "should have an associated player index" $ do
      players' !! (fromPlayerIndex $ pAction ^. actor) `shouldSatisfy` validPlayer
    it "should have an action" $ do
      pAction ^. action `shouldBe` Roll
  describe "A TradeAction" $ do
    let oneWheat = (mempty { wheat = 1 }) :: ResourceCount
    let oneOre = (mempty { ore = 1 }) :: ResourceCount

    let p1 = resources .~ mempty { wheat = 2, ore = 2 } $ mkPlayer (1, Blue, "NoOne")
    let p1offer = mkOffer (p1^.playerIndex) oneWheat oneOre
    let offer' = fromJust $ p1offer ^? action.trade.offer
    let p2 = resources .~ mempty { ore = 3 } $ mkPlayer (2, White, "Nobody")
    let p3 = mkPlayer (3, Orange, "Nadie")

    describe "A TradeOffer" $ do
      it "should have an offering amount, asking amount, and player offering" $ do
        view offering offer' `shouldBe` oneWheat
        view asking offer' `shouldBe` oneOre
        view offeredBy offer' `shouldBe` p1^.playerIndex
      it "can be checked against the resources of the player" $ do
        p1 `shouldSatisfy` (\p -> enoughFor (p1offer ^? action.trade.offer.offering) p == (Just True))

    let p2accept = accept offer' (p2^.playerIndex)
    describe "An Accept" $ do
      it "should contain the accepter, the original offer and the asker" $ do
        view actor p2accept `shouldBe` (p2^.playerIndex)
        p2accept ^? action.trade.offer `shouldBe` Just offer'
        p2accept ^? action.trade.offer.offeredBy `shouldBe` Just (p1^.playerIndex)
        p2accept ^? action.trade.accepter `shouldBe` Just (p2^.playerIndex)
      it "can be checked against the resources of the player" $ do
        p2 `shouldSatisfy` (\p -> enoughFor (p2accept ^? action.trade.offer.asking) p == (Just True))

    let p3reject = reject offer' (Just "nope") (p3^.playerIndex)
    describe "A Reject" $ do
      it "should contain the rejecter, original offer and asker" $ do
        view actor p3reject `shouldBe` p3^.playerIndex
        p3reject ^? action.trade.offer `shouldBe` Just offer'
        p3reject ^? action.trade.offer.offeredBy `shouldBe` Just (p1^.playerIndex)
        p3reject ^? action.trade.rejecter `shouldBe` Just (p3^.playerIndex)
      it "may contain a reason for rejection" $ do
        p3reject ^? action.trade.reason `shouldBe` (Just (Just "nope"))

    let p1complete = fromJust $ complete (Offer offer') (fromJust $ p2accept^? action.trade)
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
      \p -> view action (rollFor (p :: PlayerIndex)) == Roll

