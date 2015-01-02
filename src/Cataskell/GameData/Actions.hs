{-# LANGUAGE DeriveGeneric #-}

module Cataskell.GameData.Actions where

import Cataskell.GameData.Basics
import Cataskell.GameData.Resources
import Cataskell.GameData.Player

import Control.Exception (assert)
import GHC.Generics (Generic)

data TradeOffer = TradeOffer 
  { offering :: ResourceCount
  , asking :: ResourceCount
  } deriving (Eq, Show, Read,Ord, Generic)

data TradeAction
  = Offer TradeOffer
  | Accept { offer :: TradeOffer  -- ^ the offer in question
           , asker :: Player      -- ^ the original player who asked to trade
           }
  | Reject { offer :: TradeOffer     -- ^ the offer in question
           , asker :: Player         -- ^ the original player who asked to trade
           , reason :: Maybe String  -- ^ the optional reason for rejecting the trade
           }
  | CompleteTrade { offer :: TradeOffer  -- ^ the offer to be completed
                  , accepter :: Player   -- ^ the player who accepted the trade of the original offerer
                  }
  | CancelTrade { offer :: TradeOffer }  -- ^ the offer being canceled
  deriving (Eq, Show, Read,Ord, Generic)

data DiscardAction = DiscardAction
  { amountToDiscard :: Int
  , resourcesDiscarding :: ResourceCount
  } deriving (Eq, Ord, Show, Read, Generic)

data Action = Roll | Purchase Construct | Trade TradeAction | Discard DiscardAction
  deriving (Eq, Show, Read,Ord, Generic)

data PlayerAction = PlayerAction
  { actor :: Player
  , action :: Action
  } deriving (Eq, Ord, Show, Read, Generic)

getTrade :: PlayerAction -> TradeAction
getTrade (PlayerAction _ act)
  = case act of
      Trade x -> x
      _ -> error "Is not a trade!"

getOffer :: TradeAction -> TradeOffer
getOffer x
  = case x of
      Offer o -> o
      Accept o _ -> o
      Reject o _ _ -> o
      CompleteTrade o _ -> o
      CancelTrade o -> o

enoughFor :: PlayerAction -> (TradeOffer -> ResourceCount) -> Player -> Bool
enoughFor act f p
  = let tr = getTrade act
        offer' = getOffer tr
        amt = f offer'
    in sufficient (resources p) amt

mkOffer :: TradeOffer -> Player -> PlayerAction
mkOffer offer' p
  = PlayerAction
      { actor = p
      , action = Trade (Offer offer') }

accept :: PlayerAction -> Player -> PlayerAction
accept (PlayerAction original act) accepter'
  = case act of
      Trade (Offer tradeOffer) ->
        PlayerAction
          { actor = accepter'
          , action = Trade (Accept { offer = tradeOffer
                                   , asker = original })
          }
      _ -> error "Tried to accept something that wasn't an offer"

reject :: PlayerAction -> Player -> Maybe String -> PlayerAction
reject (PlayerAction original act) rejecter' maybeReason
  = case act of
      Trade (Offer tradeOffer) ->
        PlayerAction
          { actor = rejecter'
          , action = Trade (Reject { offer = tradeOffer
                                   , asker = original 
                                   , reason = maybeReason })
          }
      _ -> error "Tried to reject something that wasn't an offer"

complete :: PlayerAction -> PlayerAction -> PlayerAction
complete (PlayerAction p1 offer') (PlayerAction p2 acceptance')
  = case offer' of
      Trade (Offer tradeOffer) ->
        case acceptance' of
          Trade (Accept x _) ->
            assert (x == tradeOffer) 
              PlayerAction
                { actor = p1
                , action = Trade (CompleteTrade { offer = tradeOffer
                                                , accepter = p2
                                                })
                }
          _ -> error "The other player did not accept"
      _ -> error "There was no initial offer"

rollFor :: Player -> PlayerAction
rollFor p = PlayerAction
  { actor = p
  , action = Roll }