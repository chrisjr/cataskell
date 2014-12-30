{-# LANGUAGE DeriveGeneric #-}

module Cataskell.GameData.Player where

import Cataskell.GameData.Basics
import Cataskell.GameData.Resources
-- import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)

data Player = Player
  { color :: Color
  , resources :: ResourceCount
  , constructed :: [Construct]
  , bonuses :: [Bonus]
  } deriving (Eq, Show, Generic)

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
