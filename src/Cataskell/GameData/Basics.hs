{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Cataskell.GameData.Basics
( Valuable(..)
, Inhabited(..)
, Locatable(..)
, DevelopmentCard(..)
, Construct(..)
, unbuilt
, settlement
, city
, road
, devCard
, isNotVictoryPoint
, Terrain(..)
, Color(..)
, Bonus(..)
) where

import Cataskell.GameData.Location
import GHC.Generics (Generic)

class Valuable a where
	pointValue :: a -> Int

-- | Types of buildings that live on points
data Inhabited = Settlement | City
  deriving (Eq, Ord, Show, Generic)

-- | Types of placeable items by players
data Locatable
  = Habitation Inhabited (Maybe Point)
  | Road (Maybe UndirectedEdge)
  deriving (Eq, Ord, Show, Generic)

-- | Development cards for special actions
data DevelopmentCard = Knight | RoadBuilding | Invention | Monopoly | VictoryPoint
  deriving (Eq, Ord, Show, Generic)

data Construct
  = Building Locatable
  | DevCard (Maybe DevelopmentCard)
  deriving (Eq, Ord, Show, Generic)

unbuilt :: (Maybe a -> Construct) -> Construct
unbuilt x = x Nothing

settlement :: Maybe Point -> Construct
settlement = Building . Habitation Settlement

city :: Maybe Point -> Construct
city = Building . Habitation City

road :: Maybe UndirectedEdge -> Construct
road = Building . Road

devCard :: Maybe DevelopmentCard -> Construct
devCard = DevCard

isNotVictoryPoint :: Construct -> Bool
isNotVictoryPoint x = case x of
  DevCard (Just VictoryPoint) -> False
  _ -> True

instance Valuable Construct where
  pointValue (Building (Habitation Settlement (Just _))) = 1
  pointValue (Building (Habitation City (Just _))) = 2
  pointValue (DevCard (Just VictoryPoint)) = 1
  pointValue _ = 0

-- | Different terrains produce different resources
data Terrain = Forest | Pasture | Field | Hill | Mountain | Desert
  deriving (Eq, Ord, Show, Generic)

-- | Possible colors of player tokens
data Color = Red | Blue | Orange | White
  deriving (Eq, Ord, Show, Generic)

-- | Bonuses conferred when achieving longest road/largest army
data Bonus = LongestRoad | LargestArmy
  deriving (Eq, Ord, Show, Generic)

instance Valuable Bonus where
	pointValue _ = 2