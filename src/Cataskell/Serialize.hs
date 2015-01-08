{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module Cataskell.Serialize where

import Cataskell.SerializeOpts
import Cataskell.Game
import Cataskell.GameData.Actions
import Cataskell.GameData.Basics
import Cataskell.GameData.Board
import Cataskell.GameData.Location
import Cataskell.GameData.Player
import Cataskell.GameData.Resources
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Control.Applicative ((<$>))
import Data.Aeson
import Data.Aeson.TH

-- location instances

deriveJSON myOptions ''VertexPosition

deriveJSON myOptionsNoLens ''Point

deriveJSON myOptionsNoLens ''CentralPoint

deriveJSON myOptionsNoLens ''UndirectedEdge

-- basics
deriveJSON myOptions ''ResourceType

deriveJSON myOptionsNoLens ''ResourceCount

deriveJSON myOptions ''Terrain

deriveJSON myOptions ''Color

deriveJSON myOptions ''DevelopmentCard

deriveJSON myOptions ''Inhabited

deriveJSON myOptions ''ItemType

deriveJSON myOptions ''OnPoint

deriveJSON myOptions ''OnEdge

deriveJSON myOptions ''Construct

deriveJSON myOptions ''Item

-- board

deriveJSON myOptions ''Harbor
deriveJSON myOptions ''HexCenter

instance (ToJSON k, ToJSON a) => ToJSON (Map k a) where
  toJSON = toJSON . Map.toList

instance (Ord k, FromJSON k, FromJSON a) => FromJSON (Map k a) where
  parseJSON j = Map.fromList <$> parseJSON j

deriveJSON myOptions ''Board

-- player

deriveJSON myOptions ''PlayerIndex

deriveJSON myOptions ''Bonus

deriveJSON myOptions ''Player

-- actions

deriveJSON myOptions ''Monopoly

deriveJSON myOptions ''Invention

deriveJSON myOptions ''MoveRobber

deriveJSON myOptions ''SpecialAction

deriveJSON myOptions ''TradeOffer

deriveJSON myOptions ''TradeAction

deriveJSON myOptions ''DiscardAction

deriveJSON myOptions ''PlayerAction

deriveJSON myOptions ''GameAction

-- game state

deriveJSON myOptions ''SpecialPhase

deriveJSON myOptions ''Phase

deriveJSON myOptions ''Game
