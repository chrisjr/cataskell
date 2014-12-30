{-# LANGUAGE DeriveGeneric #-}

module Cataskell.GameData.Location
( HexCoord
, VertexPosition(..)
, Point(..)
, EdgeType(..)
, UndirectedEdge(..)
, edgeType
) where

import GHC.Generics (Generic)

-- | Axial coordinates: see http://www.redblobgames.com/grids/hexagons/
type HexCoord = (Int, Int)

data VertexPosition 
  = Top     -- ^ Top of a hex
  | Bottom  -- ^ Bottom of a hex
  | Center  -- ^ Center (location of chits/robber)
  deriving (Eq, Show, Generic)

-- | Describes a board location
data Point = Point
  { coord :: HexCoord
  , position :: VertexPosition
  } deriving (Eq, Show, Generic)

data EdgeType 
  = ToCenter  -- ^ Edge linking intersection with center (don't display)
  | Between   -- ^ Between two intersections
  deriving (Eq, Show, Generic)

data UndirectedEdge = UndirectedEdge
  { point1 :: Point
  , point2 :: Point
  } deriving (Show, Generic)

edgeType :: UndirectedEdge -> EdgeType
edgeType e
  = if any (== Center) positions then ToCenter else Between
    where positions = map position [point1 e, point2 e]

instance Eq UndirectedEdge where
  x == y = (point1 x == point1 y && point2 x == point2 y) || (point1 x == point2 y && point2 x == point1 y) 