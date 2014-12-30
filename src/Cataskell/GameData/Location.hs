{-# LANGUAGE DeriveGeneric #-}

module Cataskell.GameData.Location
( HexCoord
, hexCoords
, VertexPosition(..)
, Point(..)
, EdgeType(..)
, UndirectedEdge(..)
, dupleToEdge
, edgeType
) where

import GHC.Generics (Generic)

-- | Axial coordinates: see http://www.redblobgames.com/grids/hexagons/
type HexCoord = (Int, Int)

-- | Hexes for a board of radius 2 (>= distance 2 from center in axial coordinates)
hexCoords :: [HexCoord]
hexCoords = [ (x,y) | y <- [-2..2], x <- [-2..2], maximum [abs x, abs y, abs (x+y)] <= 2]

data VertexPosition 
  = Top     -- ^ Top of a hex
  | Bottom  -- ^ Bottom of a hex
  | Center  -- ^ Center (location of chits/robber)
  deriving (Eq, Ord, Show, Generic)

-- | Describes a board location
data Point = Point
  { coord :: HexCoord
  , position :: VertexPosition
  } deriving (Eq, Ord, Show, Generic)

data EdgeType 
  = ToCenter        -- ^ Edge linking intersection with center (don't display)
  | BetweenCenters  -- ^ Between two centers (don't display)
  | Between         -- ^ Between two intersections
  deriving (Eq, Ord, Show, Generic)

data UndirectedEdge = UndirectedEdge
  { point1 :: Point
  , point2 :: Point
  } deriving (Ord, Show, Generic)

instance Eq UndirectedEdge where
  x == y = (point1 x == point1 y && point2 x == point2 y) || (point1 x == point2 y && point2 x == point1 y) 

dupleToEdge :: (Point, Point) -> UndirectedEdge
dupleToEdge (x, y) = UndirectedEdge x y

edgeType :: UndirectedEdge -> EdgeType
edgeType e | all (== Center) positions = BetweenCenters
           | any (== Center) positions && any (/= Center) positions = ToCenter
           | otherwise = Between
    where positions = map position [point1 e, point2 e]