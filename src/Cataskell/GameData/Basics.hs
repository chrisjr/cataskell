{-# LANGUAGE DeriveGeneric #-}
module Cataskell.GameData.Basics
( Pointed(..)
, ConstructType(..)
, Construct
, Terrain(..)
, Color(..)
, DevelopmentCard(..)
, Bonus(..)
) where

import Cataskell.GameData.Location
import GHC.Generics (Generic)

class Pointed a where
	pointValue :: a -> Int

-- | Types of items built by players
data ConstructType = Road | Settlement | City | DevelopmentCard
  deriving (Eq, Show, Generic)

data Construct = OnPoint Point ConstructType | OnEdge UndirectedEdge ConstructType | Unplaced () ConstructType
  deriving (Eq, Show, Generic)

-- placed ::
-- placed

instance Pointed Construct where
	pointValue (OnPoint _ Settlement) = 1
	pointValue (OnPoint _ City) = 2
	pointValue _ = 0

-- | Different terrains produce different resources
data Terrain = Forest | Pasture | Field | Hill | Mountain | Desert
  deriving (Eq, Show, Generic)

-- | Possible colors of player tokens
data Color = Red | Blue | Orange | White
  deriving (Eq, Show, Generic)

-- | Development cards for special actions
data DevelopmentCard = Knight | RoadBuilding | Invention | Monopoly | VictoryPoint
  deriving (Eq, Show, Generic)

-- | Bonuses conferred when achieving longest road/largest army
data Bonus = LongestRoad | LargestArmy
  deriving (Eq, Show, Generic)

instance Pointed Bonus where
	pointValue _ = 2