{-# LANGUAGE DeriveGeneric #-}

module Cataskell.GameData.Location
( HexCoord
, withinHexRadius
, hexCoords
, neighborCoords
, hexNeighborhoods
, VertexPosition(..)
, Point(..)
, mkCenter
, hexCenterPoints
, EdgeType(..)
, UndirectedEdge(..)
, dupleToEdge
, edgeType
) where

import Cataskell.Util()
import Data.List (delete)
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)

-- | Axial coordinates: see http://www.redblobgames.com/grids/hexagons/
type HexCoord = (Int, Int)

withinHexRadius :: Int -> HexCoord -> ((Int, Int) -> Bool)
withinHexRadius i c 
  = let (x1, z1) = c
        y1 = (-x1) - z1
    in  \(x2, z2) -> maximum [abs (x1 - x2), abs (y1 - ((-x2) - z2)), abs (z1 - z2)] <= i

-- | Hexes for a board of radius 2 (>= distance 2 from center in axial coordinates)
hexCoords :: [HexCoord]
hexCoords = [ (x,y) | y <- [-2..2], x <- [-2..2], (withinHexRadius 2 (0, 0)) (x, y)]

-- | Return a coordinate's neighbors (restricted to actually existing coords)
neighborCoords :: HexCoord -> [HexCoord]
neighborCoords c
  = let lst = [ (x,y) + c | y <- [-1..1], x <- [-1..1], (withinHexRadius 1 c) $ (x, y) + c]
    in  filter (\x -> x /= c && x `elem` hexCoords) lst

hexNeighborhoods :: Map.Map HexCoord [HexCoord]
hexNeighborhoods = Map.fromList hexN
  where hexN = zip hexCoords nns
        nns = map getNs hexCoords
        getNs hc = let nc = neighborCoords hc
                   in  delete hc nc

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

mkCenter :: HexCoord -> Point
mkCenter hexCoord = Point { coord = hexCoord, position = Center }

hexCenterPoints :: [Point]
hexCenterPoints = map mkCenter hexCoords

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