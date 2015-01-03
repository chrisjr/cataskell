{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Cataskell.GameData.Actions where

import Cataskell.GameData.Basics
import Cataskell.GameData.Board
import Cataskell.GameData.Location
import Cataskell.GameData.Resources
import Cataskell.GameData.Player

import Control.Lens
import Control.Exception (assert)
import GHC.Generics (Generic)

data TradeOffer = TradeOffer 
  { _offering :: ResourceCount
  , _asking :: ResourceCount
  } deriving (Eq, Show, Read, Ord, Generic)

makeLenses ''TradeOffer

data TradeAction
  = Offer { _offer :: TradeOffer }
  | Accept { _offer :: TradeOffer  -- ^ the offer in question
           , _asker :: Player      -- ^ the original player who asked to trade
           }
  | Reject { _offer :: TradeOffer     -- ^ the offer in question
           , _asker :: Player         -- ^ the original player who asked to trade
           , _reason :: Maybe String  -- ^ the optional reason for rejecting the trade
           }
  | CompleteTrade { _offer :: TradeOffer  -- ^ the offer to be completed
                  , _accepter :: Player   -- ^ the player who accepted the trade of the original offerer
                  }
  | CancelTrade { _offer :: TradeOffer }  -- ^ the offer being canceled
  deriving (Eq, Show, Read, Ord, Generic)

makeLenses ''TradeAction

data DiscardAction = DiscardAction
  { _amountToDiscard :: Int
  , _resourcesDiscarding :: ResourceCount
  } deriving (Eq, Ord, Show, Read, Generic)

makeLenses ''DiscardAction

data PlayerAction
  = Roll
  | BuildForFree { _building :: Construct }
  | Purchase { _building :: Construct }
  | Trade { _trade :: TradeAction }
  | Discard { _discarding :: DiscardAction }
  deriving (Eq, Show, Read,Ord, Generic)

makeLenses ''PlayerAction

data GameAction
  = PlayerAction { _actor :: Player
                 , _action :: PlayerAction }
  deriving (Eq, Ord, Show, Read, Generic)

makeLenses ''GameAction

mkInitialSettlement :: (Player, Point) -> GameAction
mkInitialSettlement (player', point')
  = PlayerAction { _actor = player'
                 , _action = BuildForFree (settlement $ Just (point', color player')) }

possibleInitialSettlements :: Player -> Board -> [GameAction]
possibleInitialSettlements p b
  = let ps = freePoints b
    in  map mkInitialSettlement $ zip (repeat p) ps

-- | Enough resources for something
enoughFor :: Maybe ResourceCount -> Player -> Maybe Bool
enoughFor amt p
  = (sufficient (resources p)) `fmap` amt

mkOffer :: TradeOffer -> Player -> GameAction
mkOffer offer' p
  = PlayerAction
      { _actor = p
      , _action = Trade (Offer offer') }

accept :: GameAction -> Player -> GameAction
accept (PlayerAction original act') accepter'
  = case act' of
      Trade (Offer tradeOffer) ->
        PlayerAction
          { _actor = accepter'
          , _action = Trade (Accept { _offer = tradeOffer
                                    , _asker = original })
          }
      _ -> error "Tried to accept something that wasn't an offer"

reject :: GameAction -> Player -> Maybe String -> GameAction
reject (PlayerAction original act') rejecter' maybeReason
  = case act' of
      Trade (Offer tradeOffer) ->
        PlayerAction
          { _actor = rejecter'
          , _action = Trade (Reject { _offer = tradeOffer
                                    , _asker = original 
                                    , _reason = maybeReason })
          }
      _ -> error "Tried to reject something that wasn't an offer"

complete :: GameAction -> GameAction -> GameAction
complete (PlayerAction p1 offer') (PlayerAction p2 acceptance')
  = case offer' of
      Trade (Offer tradeOffer) ->
        case acceptance' of
          Trade (Accept x _) ->
            assert (x == tradeOffer) 
              PlayerAction
                { _actor = p1
                , _action = Trade (CompleteTrade { _offer = tradeOffer
                                                 , _accepter = p2
                                                })
                }
          _ -> error "The other player did not accept"
      _ -> error "There was no initial offer"

rollFor :: Player -> GameAction
rollFor p = PlayerAction
  { _actor = p
  , _action = Roll }