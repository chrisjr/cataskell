{-# LANGUAGE DeriveGeneric #-}

module Cataskell.GameData.Actions where

import Cataskell.GameData.Basics
import Cataskell.GameData.Resources
import Cataskell.GameData.Player

import GHC.Generics (Generic)

data TradeOffer = TradeOffer 
  { offering :: ResourceCount
  , asking :: ResourceCount
  } deriving (Eq, Show, Ord, Generic)

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
  deriving (Eq, Show, Ord, Generic)

data DiscardAction = DiscardAction
  { amountToDiscard :: Int
  , resourcesDiscarding :: ResourceCount
  } deriving (Eq, Show, Ord, Generic)

data Action = Purchase Construct | Trade TradeAction | Discard DiscardAction
  deriving (Eq, Show, Ord, Generic)

data PlayerAction = PlayerAction
  { actor :: Player
  , action :: Action
  } deriving (Eq, Show, Ord, Generic)
