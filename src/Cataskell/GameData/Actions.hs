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
import GHC.Generics (Generic)

data TradeOffer = TradeOffer 
  { _offering :: ResourceCount
  , _asking :: ResourceCount
  , _offeredBy :: PlayerIndex  -- ^ the original player making the offer
  } deriving (Eq, Show, Read, Ord, Generic)

makeLenses ''TradeOffer

data TradeAction
  = Offer { _offer :: TradeOffer }
  | Accept { _offer :: TradeOffer      -- ^ the offer in question
           , _accepter :: PlayerIndex  -- ^ the player accepting
           }
  | Reject { _offer :: TradeOffer     -- ^ the offer in question
           , _rejecter :: PlayerIndex -- ^ the player rejecting
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

data Monopoly = MonopolyOn { _resourceType :: ResourceType }
  deriving (Eq, Ord, Show, Read, Generic)

makeLenses ''Monopoly

data Invention = InventionOf { _resourceCount :: ResourceCount }
  deriving (Eq, Ord, Show, Read, Generic)

makeLenses ''Invention

data MoveRobber = MoveRobber { _destination :: CentralPoint }
  deriving (Eq, Ord, Show, Read, Generic)

makeLenses ''MoveRobber

data SpecialAction
  = M { _monopoly :: Monopoly }
  | I { _invention :: Invention }
  | R { _moveRobber :: MoveRobber }
  deriving (Eq, Ord, Show, Read, Generic)

makeLenses ''SpecialAction

data PlayerAction
  = Roll
  | BuildForFree { _construct :: Construct }
  | SpecialAction { _specialAction :: SpecialAction }
  | PlayCard { _cardToPlay :: DevelopmentCard }
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

-- | Test if the action is a TradeAction.
isTradeAction :: GameAction -> Bool
isTradeAction trade' = case trade' of
  PlayerAction _ (Trade _) -> True
  _ -> False

toOffer :: TradeAction -> Maybe TradeOffer
toOffer trade' = case trade' of
  Offer x -> Just x
  _ -> Nothing

-- | Create a Discard action
mkDiscard :: (PlayerIndex, ResourceCount) -> GameAction
mkDiscard (pI, res)
  = PlayerAction { _actor = pI
                 , _action = Discard DiscardAction
                    { _amountToDiscard = totalResources res
                    , _resourcesDiscarding = res
                    } 
                 }

mkInitialSettlement :: (Player, Point) -> GameAction
mkInitialSettlement (player', p) = mkFree (player'^.playerIndex) $ mkSettlement (player', p)

mkFree :: PlayerIndex -> Construct -> GameAction
mkFree pI construct' = PlayerAction { _actor = pI
                                    , _action = BuildForFree construct' }

purchase :: PlayerIndex -> Item -> GameAction
purchase pI item' = PlayerAction { _actor = pI
                                      , _action = Purchase item' }

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

mkOffer :: PlayerIndex -> ResourceCount -> ResourceCount -> GameAction
mkOffer playerIndex' offering' asking'
  = PlayerAction
      { _actor = playerIndex'
      , _action = Trade (Offer (TradeOffer offering' asking' playerIndex'))
      }

mkPlayCard :: PlayerIndex -> DevelopmentCard -> GameAction
mkPlayCard playerIndex' card'
  = PlayerAction
      { _actor = playerIndex'
      , _action = PlayCard card' }

accept :: TradeOffer -> PlayerIndex -> GameAction
accept offer' accepter'
  = PlayerAction
      { _actor = accepter'
      , _action = Trade (Accept { _offer = offer'
                                , _accepter = accepter' }) }

reject :: TradeOffer -> Maybe String -> PlayerIndex -> GameAction
reject offer' maybeReason rejecter'
  = PlayerAction
      { _actor = rejecter'
      , _action = Trade (Reject { _offer = offer'
                                , _rejecter = rejecter'
                                , _reason = maybeReason })
      }

cancel :: TradeOffer -> GameAction
cancel o@(TradeOffer _ _ pI)
  = PlayerAction
      { _actor = pI
      , _action = Trade (CancelTrade o) }

complete :: TradeAction -> Maybe GameAction
complete acceptance = do
  let offer' = acceptance ^. offer
  let p1 = offer' ^. offeredBy
  p2 <- acceptance ^? accepter
  return $ PlayerAction
      { _actor = p1
      , _action = Trade (CompleteTrade { _offer = offer'
                                       , _accepter = p2
                                       })
      }

rollFor :: PlayerIndex -> GameAction
rollFor pI = PlayerAction
  { _actor = pI
  , _action = Roll }

invent :: PlayerIndex -> ResourceCount -> GameAction
invent playerIndex' res'
  = let act' = SpecialAction . I $ InventionOf res'
    in PlayerAction { _actor = playerIndex'
                    , _action = act' }

resCombinationsForTotal :: Int -> [ResourceCount]
resCombinationsForTotal i = map (\(a,b,c,d,e) -> ResourceCount a b c d e) tuples
  where tuples = [ (a,b,c,d,e) | a <- is, b <- is, c <- is, d <- is, e <- is, sum [a,b,c,d,e] == i]
        is = [0..i]

possibleInventions :: [ResourceCount]
possibleInventions = resCombinationsForTotal 2

possibleMonopolies :: [SpecialAction]
possibleMonopolies = map (M . MonopolyOn) resTypes
  where resTypes = [Lumber, Ore, Wool, Wheat, Brick]

mkEndTurn :: PlayerIndex -> GameAction
mkEndTurn pI = PlayerAction
  { _actor = pI
  , _action = EndTurn }

mkMoveRobber :: PlayerIndex -> CentralPoint -> GameAction
mkMoveRobber pI dest = PlayerAction
  { _actor = pI
  , _action = SpecialAction (R (MoveRobber dest)) }
