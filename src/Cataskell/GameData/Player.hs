{-# LANGUAGE DeriveGeneric #-}

module Cataskell.GameData.Player
( Player
, color
, resources
, constructed
, bonuses
, mkPlayer
, validPlayer
, displayScore
, score
, devCards
) where

import Cataskell.GameData.Basics
import Cataskell.GameData.Resources
import Data.Monoid (mempty)
import Data.Maybe
import GHC.Generics (Generic)

data Player = Player
  { playerColor :: Color
  , resources :: ResourceCount
  , constructed :: [ActualItem]
  , bonuses :: [Bonus]
  } deriving (Eq, Show, Read,Ord, Generic)

instance Colored Player where
  color = playerColor

mkPlayer :: Color -> Player
mkPlayer c = Player
  { playerColor = c
  , resources = mempty
  , constructed = []
  , bonuses = []
  }

validPlayer :: Player -> Bool
validPlayer p
  = let resourcesNonNegative = nonNegative $ resources p
        allBuildingsColoredRight = all (== color p) . map color . catMaybes . map getBuilding $ constructed p
    in resourcesNonNegative && allBuildingsColoredRight

totalPointsOf :: Valuable a => [a] -> Int
totalPointsOf = sum . map pointValue

displayScore :: Player -> Int
displayScore p = regularPoints + bonusPoints
  where regularPoints = totalPointsOf . filter (not . isVictoryPoint) $ constructed p
        bonusPoints   = totalPointsOf $ bonuses p

score :: Player -> Int
score p = displayScore p + victoryPointCards
  where victoryPointCards = totalPointsOf . filter isVictoryPoint $ constructed p

devCards :: Player -> [DevelopmentCard]
devCards = concatMap getDevCard . constructed
  where getDevCard c = case c of
          Card x -> [x]
          _ -> []