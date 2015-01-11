{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Cataskell.GameData.Player where

import Cataskell.GameData.Basics
import Cataskell.GameData.Resources
import Data.Monoid (mempty)
import Data.Maybe
import Control.Lens
import GHC.Generics (Generic)

newtype PlayerIndex = PlayerIndex Int
  deriving (Eq, Ord, Show, Read, Generic)

toPlayerIndex :: Int -> PlayerIndex
toPlayerIndex i | i <= 3 = PlayerIndex i
                | otherwise = error $ show i ++ " out of bounds"

fromPlayerIndex :: PlayerIndex -> Int
fromPlayerIndex x = case x of
  PlayerIndex i -> i

data Player = Player
  { _playerName :: String
  , _playerColor :: Color
  , _playerIndex :: PlayerIndex
  , _resources :: ResourceCount
  , _constructed :: [Item]
  , _newCards :: [DevelopmentCard]
  , _bonuses :: [Bonus]
  } deriving (Eq, Ord, Show, Read, Generic)

makeLenses ''Player

instance Colored Player where
  color = view playerColor

mkPlayer :: (Int, Color, String) -> Player
mkPlayer (i, c, n) = Player
  { _playerName = n
  , _playerColor = c
  , _playerIndex = toPlayerIndex i
  , _resources = mempty
  , _constructed = initialItems
  , _newCards = []
  , _bonuses = []
  }

mkPlayers :: [String] -> [Player]
mkPlayers = map mkPlayer . zip3 [0..] [Red, Blue, Orange, White]

validPlayer :: Player -> Bool
validPlayer p
  = let resourcesNonNegative = views resources nonNegative p
        bldgs = mapMaybe (preview building) (p ^. constructed)
        allBuildingsColoredRight = all (== color p) . map color $ bldgs
    in resourcesNonNegative && allBuildingsColoredRight

totalPointsOf :: Valuable a => [a] -> Int
totalPointsOf = sum . map pointValue

displayScore :: Getter Player Int
displayScore = to displayScore'
  where displayScore' p = regularPoints p + bonusPoints p
        regularPoints p = totalPointsOf . filter (not . isVictoryPoint) $ p ^. constructed
        bonusPoints = views bonuses totalPointsOf

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
