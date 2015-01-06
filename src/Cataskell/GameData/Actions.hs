{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Cataskell.GameData.Actions where

import Cataskell.GameData.Basics
import Cataskell.GameData.Board
import Cataskell.GameData.Location
import Cataskell.GameData.Resources
import Cataskell.GameData.Player

import Control.Lens
import Data.Monoid (mempty)
import qualified Data.Map.Strict as Map
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
           , _asker :: PlayerIndex      -- ^ the original player who asked to trade
           }
  | Reject { _offer :: TradeOffer     -- ^ the offer in question
           , _asker :: PlayerIndex         -- ^ the original player who asked to trade
           , _reason :: Maybe String  -- ^ the optional reason for rejecting the trade
           }
  | CompleteTrade { _offer :: TradeOffer  -- ^ the offer to be completed
                  , _accepter :: PlayerIndex   -- ^ the player who accepted the trade of the original offerer
                  }
  | CancelTrade { _offer :: TradeOffer }  -- ^ the offer being canceled
  | Exchange { _offer :: TradeOffer }     -- ^ exchange with any harbor benefits applied
  deriving (Eq, Show, Read, Ord, Generic)

makeLenses ''TradeAction

data DiscardAction = DiscardAction
  { _amountToDiscard :: Int
  , _resourcesDiscarding :: ResourceCount
  } deriving (Eq, Ord, Show, Read, Generic)

makeLenses ''DiscardAction

data PlayerAction
  = Roll
  | BuildForFree { _construct :: Construct }
  | PlayCard { _card :: DevelopmentCard }
  | Purchase { _item :: Item }
  | Trade { _trade :: TradeAction }
  | Discard { _discarding :: DiscardAction }
  | EndTurn
  deriving (Eq, Show, Read,Ord, Generic)

makeLenses ''PlayerAction

data GameAction
  = PlayerAction { _actor :: PlayerIndex
                 , _action :: PlayerAction }
  deriving (Eq, Ord, Show, Read, Generic)

makeLenses ''GameAction

-- | Create an empty Discard action
mkDiscard :: (PlayerIndex, Int) -> GameAction
mkDiscard (pI, currentTotal)
  = PlayerAction { _actor = pI
                 , _action = Discard DiscardAction
                    { _amountToDiscard = currentTotal `div` 2
                    , _resourcesDiscarding = mempty 
                    } 
                 }

mkInitialSettlement :: (Player, Point) -> GameAction
mkInitialSettlement (player', p) = mkFree (player'^.playerIndex) $ mkSettlement (player', p)

mkFree :: PlayerIndex -> Construct -> GameAction
mkFree pI construct' = PlayerAction { _actor = pI
                                    , _action = BuildForFree construct' }

mkSettlement :: (Player, Point) -> Construct
mkSettlement (player', p') = built . settlement $ Just (p', color player')

possibleInitialSettlements :: Player -> Board -> [GameAction]
possibleInitialSettlements player' b
  = let ps = freePoints b
    in  map mkInitialSettlement $ zip (repeat player') ps

initialRoadsFor :: Player -> OnPoint -> RoadMap -> [GameAction]
initialRoadsFor player' o' roads'
  = let point' = o' ^.point
        roadEdges = filter (\k -> point1 k == point' || point2 k == point') $ Map.keys roads' 
        c' = color player'
    in  map (\e -> mkFree (player'^.playerIndex) $ built . road $ Just (e,c')) roadEdges

-- | Enough resources for something
enoughFor :: Maybe ResourceCount -> Player -> Maybe Bool
enoughFor amt p
  = (sufficient (p ^. resources)) `fmap` amt

mkOffer :: TradeOffer -> Player -> GameAction
mkOffer offer' p
  = PlayerAction
      { _actor = p^.playerIndex
      , _action = Trade (Offer offer') }

accept :: (Monad m) => GameAction -> PlayerIndex -> m GameAction
accept act' accepterI
  = let offer' = act' ^? action.trade.offer
    in case offer' of
      Just tradeOffer ->
        return PlayerAction
          { _actor = accepterI
          , _action = Trade (Accept { _offer = tradeOffer
                                    , _asker = act' ^. actor }) }
      Nothing -> fail "Tried to accept something that wasn't an offer"

reject :: (Monad m) => GameAction -> PlayerIndex -> Maybe String -> m GameAction
reject act' rejecterIndex maybeReason
  = let offer' = act' ^? action.trade.offer
    in case offer' of
      Just tradeOffer ->
        return PlayerAction
          { _actor = rejecterIndex
          , _action = Trade (Reject { _offer = tradeOffer
                                    , _asker = act' ^. actor 
                                    , _reason = maybeReason })
          }
      Nothing -> fail "Tried to reject something that wasn't an offer"

complete :: GameAction -> GameAction -> Maybe GameAction
complete p1offer p2accept = do
  offer' <- p1offer ^? action.trade.offer
  offer'' <- p2accept ^? action.trade.offer
  let p1 = p1offer ^. actor
  let p2 = p2accept ^. actor
  return $ assert (offer' == offer'') PlayerAction
      { _actor = p1
      , _action = Trade (CompleteTrade { _offer = offer'
                                       , _accepter = p2
                                      })
      }

rollFor :: PlayerIndex -> GameAction
rollFor pI = PlayerAction
  { _actor = pI
  , _action = Roll }