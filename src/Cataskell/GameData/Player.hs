{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Cataskell.GameData.Player
( Player
, playerName
, playerIndex
, resources
, constructed
, bonuses
, mkPlayer
, mkPlayers
, validPlayer
, displayScore
, score
, devCards
) where

import Cataskell.GameData.Basics
import Cataskell.GameData.Resources
import Data.Monoid (mempty)
import Data.Maybe
import Control.Lens
import GHC.Generics (Generic)

data Player = Player
  { _playerName :: String
  , _playerColor :: Color
  , _playerIndex :: Int
  , _resources :: ResourceCount
  , _constructed :: [Item]
  , _bonuses :: [Bonus]
  } deriving (Eq, Show, Read,Ord, Generic)

makeLenses ''Player

instance Colored Player where
  color = view playerColor

mkPlayer :: (Int, Color, String) -> Player
mkPlayer (i, c, n) = Player
  { _playerName = n
  , _playerColor = c
  , _playerIndex = i
  , _resources = mempty
  , _constructed = initialItems
  , _bonuses = []
  }

mkPlayers :: [String] -> [Player]
mkPlayers = map mkPlayer . zip3 [0..] [Red, Blue, Orange, White]

validPlayer :: Player -> Bool
validPlayer p
  = let resourcesNonNegative = views resources nonNegative p
        bldgs = catMaybes . map (preview building) $ (p ^. constructed)
        allBuildingsColoredRight = all (== color p) . map color $ bldgs
    in resourcesNonNegative && allBuildingsColoredRight

totalPointsOf :: Valuable a => [a] -> Int
totalPointsOf = sum . map pointValue

displayScore :: Getter Player Int
displayScore = to displayScore'
  where displayScore' p = regularPoints p + bonusPoints p
        regularPoints p = totalPointsOf . filter (not . isVictoryPoint) $ p ^. constructed
        bonusPoints   p = views bonuses totalPointsOf p

score :: Getter Player Int
score = to score'
  where score' p = view displayScore p + victoryPointCards p
        victoryPointCards p = totalPointsOf . filter isVictoryPoint $ p ^. constructed

devCards :: Getter Player [DevelopmentCard]
devCards = to devCards'
  where devCards' = concatMap getDevCard . view constructed
        getDevCard c = case c of
          Card x -> [x]
          _ -> []