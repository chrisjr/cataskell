{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Cataskell.GameData.Basics
( Valuable(..)
, DevelopmentCard(..)
, Inhabited(..)
, ItemType(..)
, OnPoint(..)
, OnEdge(..)
, Construct(..)
, Item(..)
, itemType
, building
, point
, edge
, buildingType
, onPoint
, onEdge
, unbuilt
, built
, settlement
, city
, road
, devCard
, deconstruct
, isSettlement
, isVictoryPoint
, Terrain(..)
, ResourceType(..)
, Harbor(..)
, Color(..)
, Colored(..)
, Bonus(..)
, initialItems
, allDevelopmentCards
) where

import Cataskell.GameData.Location
import Control.Lens
import Data.Maybe (fromJust)
import GHC.Generics (Generic)

-- | Possible colors of player tokens
data Color = Red | Blue | Orange | White
  deriving (Eq, Ord, Show, Read,Generic)

-- | Different terrains produce different resources
data Terrain = Forest | Pasture | Field | Hill | Mountain | Desert
  deriving (Eq, Ord, Show, Read,Generic)

-- | Different resources (see ResourceCount for most uses)
data ResourceType = Lumber | Wool | Wheat | Brick | Ore
  deriving (Eq, Ord, Show, Read,Generic)

-- | Harbors allow exchanging 2 of a terrain's resource type for anything, or 3 of anything for 1 anything else
data Harbor = Harbor Terrain | ThreeToOne
  deriving (Eq, Ord, Show, Read, Generic)

-- | Class of items with point value
class Valuable a where
  pointValue :: a -> Int

-- | Class of items that may have a color
class Colored a where
  color :: a -> Color

-- | Development cards for special actions
data DevelopmentCard = Knight | RoadBuilding | Invention | Monopoly | VictoryPoint
  deriving (Eq, Ord, Show, Read, Generic)

data Inhabited = Settlement | City
  deriving (Eq, Ord, Show, Read, Generic)

data ItemType = H Inhabited | Road | DevelopmentCard
  deriving (Eq, Ord, Show, Read, Generic)

data OnPoint = OnPoint { _point :: Point, _pointColor :: Color, _buildingType :: Inhabited }
  deriving (Eq, Ord, Show, Read, Generic)

data OnEdge = OnEdge { _edge :: UndirectedEdge, _edgeColor :: Color }
  deriving (Eq, Ord, Show, Read, Generic)

data Construct = Edifice { _onPoint :: OnPoint } | Roadway { _onEdge :: OnEdge }
  deriving (Eq, Ord, Show, Read, Generic)

data Item
  = Building { _building :: Construct }
  | Card { _card :: DevelopmentCard }
  | Potential { _itemType :: ItemType }
  deriving (Eq, Ord, Show, Read, Generic)

makeLenses ''OnPoint
makeLenses ''OnEdge
makeLenses ''Construct
makeLenses ''Item

instance Colored OnPoint where
  color = view pointColor

instance Colored OnEdge where
  color = view edgeColor

instance Colored Construct where
  color (Edifice p) = p ^. pointColor
  color (Roadway e) = e ^. edgeColor

unbuilt :: (Maybe a -> Item) -> Item
unbuilt = ($ Nothing)

built :: Item -> Construct
built = fromJust . preview building

settlement :: Maybe (Point, Color) -> Item
settlement x = case x of
  Just (p, c) -> Building (Edifice (OnPoint p c Settlement))
  Nothing -> Potential (H Settlement)

city :: Maybe (Point, Color) -> Item
city x = case x of
  Just (p, c) -> Building (Edifice (OnPoint p c City))
  Nothing -> Potential (H City)

road :: Maybe (UndirectedEdge, Color) -> Item
road x = case x of
  Just (e, c) -> Building (Roadway (OnEdge e c))
  Nothing -> Potential Road

devCard :: Maybe DevelopmentCard -> Item
devCard  x = case x of
  Just d -> Card d
  Nothing -> Potential DevelopmentCard

deconstruct :: Construct -> Item
deconstruct c'
  = case c' of
      Edifice (OnPoint _ _ h) -> Potential (H h)
      Roadway (OnEdge _ _) -> Potential Road

isSettlement :: Item -> Bool
isSettlement item' = case item' of
  Building (Edifice (OnPoint _ _ Settlement)) -> True
  _ -> False

isVictoryPoint :: Item -> Bool
isVictoryPoint x = case (x ^? card) of
  Just VictoryPoint -> True
  _ -> False

instance Valuable Item where
  pointValue (Building (Edifice (OnPoint _ _ Settlement))) = 1
  pointValue (Building (Edifice (OnPoint _ _ City))) = 2
  pointValue (Card VictoryPoint) = 1
  pointValue _ = 0

-- | Bonuses conferred when achieving longest road/largest army
data Bonus = LongestRoad | LargestArmy
  deriving (Eq, Ord, Show, Read,Generic)

initialItems :: [Item]
initialItems = settlements ++ cities ++ roads
  where settlements = replicate 5 (Potential (H Settlement))
        cities = replicate 4 (Potential (H City))
        roads = replicate 15 (Potential Road)

allDevelopmentCards :: [Item]
allDevelopmentCards = knights ++ vps ++ roadBuilds ++ inventions ++ monopolies
  where knights = replicate 14 (Card Knight)
        vps = replicate 5 (Card VictoryPoint)
        roadBuilds = replicate 2 (Card RoadBuilding)
        inventions = replicate 2 (Card Invention)
        monopolies = replicate 2 (Card Monopoly)

instance Valuable Bonus where
  pointValue _ = 2