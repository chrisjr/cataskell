{-# LANGUAGE DeriveGeneric #-}

module Cataskell.GameData.Actions where

import Cataskell.GameData.Basics
import Cataskell.GameData.Resources
import Cataskell.GameData.Player

import GHC.Generics (Generic)

data TradeAction = TradeAction
  { given :: ResourceCount
  , received :: ResourceCount
  } deriving (Eq, Show, Ord, Generic)

data Action = Build Construct | Trade TradeAction | Discard ResourceCount
  deriving (Eq, Show, Ord, Generic)

data PlayerAction = PlayerAction
  { player :: Player
  , action :: Action
  } deriving (Eq, Show, Ord, Generic)
