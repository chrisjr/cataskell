{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Cataskell.GameData.Basics
( Valuable(..)
, Inhabited(..)
, Road(..)
, PotentialBuilding(..)
, DevelopmentCard(..)
, PotentialItem(..)
, ActualHabitation(..)
, ActualRoad(..)
, ActualBuilding(..)
, ActualItem(..)
, Construct(..)
, unbuilt
, settlement
, city
, road
, devCard
, isVictoryPoint
, getBuilding
, Terrain(..)
, Color(..)
, Colored(..)
, Bonus(..)
) where

import Cataskell.GameData.Location
import GHC.Generics (Generic)

-- | Possible colors of player tokens
data Color = Red | Blue | Orange | White
  deriving (Eq, Ord, Show, Generic)

-- | Different terrains produce different resources
data Terrain = Forest | Pasture | Field | Hill | Mountain | Desert
  deriving (Eq, Ord, Show, Generic)

-- | Class of items with point value
class Valuable a where
  pointValue :: a -> Int

-- | Class of items that may have a color
class Colored a where
  color :: a -> Color

-- | Types of buildings that live on points
data Inhabited = Settlement | City
  deriving (Eq, Ord, Show, Generic)

data Road = Road
  deriving (Eq, Ord, Show, Generic)

-- | Types of placeable items by players (if placed, must also have a player color)
data PotentialBuilding
  = HabitationToBe Inhabited
  | RoadToBe Road
  deriving (Eq, Ord, Show, Generic)

-- | Development cards for special actions
data DevelopmentCard = Knight | RoadBuilding | Invention | Monopoly | VictoryPoint
  deriving (Eq, Ord, Show, Generic)

data PotentialItem = Potential PotentialBuilding | DevCard
  deriving (Eq, Ord, Show, Generic)

data ActualHabitation = H Inhabited Point Color
  deriving (Eq, Ord, Show, Generic)

data ActualRoad = R Road UndirectedEdge Color
  deriving (Eq, Ord, Show, Generic)

data ActualBuilding
  = OnPoint ActualHabitation
  | OnEdge ActualRoad
  deriving (Eq, Ord, Show, Generic)

data ActualItem
  = Building ActualBuilding
  | Card DevelopmentCard
  deriving (Eq, Ord, Show, Generic)

data Construct
  = Built ActualItem
  | Unbuilt PotentialItem
  deriving (Eq, Ord, Show, Generic)

instance Colored ActualBuilding where
  color (OnPoint (H _ _ c)) = c
  color (OnEdge (R _ _ c)) = c

unbuilt :: (Maybe a -> Construct) -> PotentialItem
unbuilt x = case x Nothing of
  Unbuilt p -> p
  Built _ -> error "Can't get PotentialItem from ActualItem"

settlement :: Maybe (Point, Color) -> Construct
settlement x = case x of
  Just (p, c) -> Built . Building . OnPoint $ H Settlement p c
  Nothing -> Unbuilt . Potential $ HabitationToBe Settlement

city :: Maybe (Point, Color) -> Construct
city x = case x of
  Just (p, c) -> Built . Building . OnPoint $ H City p c
  Nothing -> Unbuilt . Potential $ HabitationToBe City

road :: Maybe (UndirectedEdge, Color) -> Construct
road x = case x of
  Just (e, c) -> Built . Building $ OnEdge $ R Road e c
  Nothing -> Unbuilt . Potential $ RoadToBe Road

devCard :: Maybe DevelopmentCard -> Construct
devCard  x = case x of
  Just d -> Built $ Card d
  Nothing -> Unbuilt $ DevCard

isVictoryPoint :: ActualItem -> Bool
isVictoryPoint x = case x of
  Card VictoryPoint -> True
  _ -> False

getBuilding :: ActualItem -> Maybe ActualBuilding
getBuilding x = case x of
  Building y -> Just y
  Card _ -> Nothing

instance Valuable ActualItem where
  pointValue (Building (OnPoint (H Settlement _ _))) = 1
  pointValue (Building (OnPoint (H City _ _))) = 2
  pointValue (Card VictoryPoint) = 1
  pointValue _ = 0

-- | Bonuses conferred when achieving longest road/largest army
data Bonus = LongestRoad | LargestArmy
  deriving (Eq, Ord, Show, Generic)

instance Valuable Bonus where
  pointValue _ = 2