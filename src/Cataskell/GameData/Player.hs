{-# LANGUAGE DeriveGeneric #-}

module Cataskell.GameData.Player
( Player
, color
, resources
, constructed
, bonuses
, player
, getDisplayScore
, getScore
) where

import Cataskell.GameData.Basics
import Cataskell.GameData.Resources
import Data.Monoid (mempty)
import GHC.Generics (Generic)

data Player = Player
  { color :: Color
  , resources :: ResourceCount
  , constructed :: [Construct]
  , bonuses :: [Bonus]
  } deriving (Eq, Show, Generic)

player :: Color -> Player
player c = Player
  { color = c
  , resources = mempty
  , constructed = []
  , bonuses = []
  }

getDisplayScore :: Player -> Int
getDisplayScore p = regularPoints + bonusPoints
  where regularPoints = totalPointsOf . filter (isNotVictoryPoint) $ constructed p
        bonusPoints   = totalPointsOf $ bonuses p
        totalPointsOf x = sum $ map pointValue x


getScore :: Player -> Int
getScore p = regularPoints + bonusPoints
  where regularPoints = totalPointsOf $ constructed p
        bonusPoints   = totalPointsOf $ bonuses p
        totalPointsOf x = sum $ map pointValue x
