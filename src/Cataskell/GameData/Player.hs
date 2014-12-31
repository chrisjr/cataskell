{-# LANGUAGE DeriveGeneric #-}

module Cataskell.GameData.Player
( Player
, color
, resources
, constructed
, bonuses
, player
, displayScore
, score
, devCards
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
          DevCard (Just x) -> [x]
          _ -> []