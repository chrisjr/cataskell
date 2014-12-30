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
  deriving (Eq, Show, Generic)

-- | Types of placeable items by players
data Locatable
  = Habitation Inhabited (Maybe Point)
  | Road (Maybe UndirectedEdge)
  deriving (Eq, Show, Generic)

-- | Development cards for special actions
data DevelopmentCard = Knight | RoadBuilding | Invention | Monopoly | VictoryPoint
  deriving (Eq, Show, Generic)

data Construct
  = Building Locatable
  | DevCard (Maybe DevelopmentCard)
  deriving (Eq, Show, Generic)

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
  deriving (Eq, Show, Generic)

-- | Possible colors of player tokens
data Color = Red | Blue | Orange | White
  deriving (Eq, Show, Generic)

-- | Bonuses conferred when achieving longest road/largest army
data Bonus = LongestRoad | LargestArmy
  deriving (Eq, Show, Generic)

instance Valuable Bonus where
	pointValue _ = 2