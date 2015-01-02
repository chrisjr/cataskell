{-# LANGUAGE DeriveGeneric #-}

module Cataskell.GameData.Location
( HexCoord
, withinHexRadius
, hexCoords
, neighborCoords
, hexNeighborhoods
, VertexPosition(..)
, Point(..)
, CentralPoint(..)
, toCenter
, fromCenter
, mkCenter
, hexCenterPoints
, pointsAroundHex
, allPoints
, mkHexPointsAndEdges
, EdgeType(..)
, UndirectedEdge(..)
, dupleToEdge
, edgeType
) where

import Cataskell.Util
import Data.List (delete, nub)
import Data.Maybe (fromJust)
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

hexNeighborhoods :: Map.Map Point [Point]
hexNeighborhoods = Map.fromList hexN
  where hexN = zip hexCenterPoints nns
        nns = map (map mkCenter) $ map getNs hexCoords
        getNs hc = let nc = neighborCoords hc
                   in  delete hc nc

data VertexPosition 
  = Top     -- ^ Top of a hex
  | Bottom  -- ^ Bottom of a hex
  | Center  -- ^ Center (location of chits/robber)
  deriving (Eq, Ord, Show, Read,Generic)

-- | Describes a board location
data Point = Point
  { coord :: HexCoord
  , position :: VertexPosition
  } deriving (Eq, Ord, Show, Read,Generic)

newtype CentralPoint = Central Point
 deriving ( Eq, Ord, Show, Read )

toCenter :: Point -> CentralPoint
toCenter x | position x == Center = Central x
           | otherwise = error "Point is not a center"

fromCenter :: CentralPoint -> Point
fromCenter (Central p) = p

mkCenter :: HexCoord -> Point
mkCenter hexCoord = Point { coord = hexCoord, position = Center }

hexCenterPoints :: [Point]
hexCenterPoints = map mkCenter hexCoords

-- | Vertices around the edge of a given hex.
pointsAroundHex :: CentralPoint -> [Point]
pointsAroundHex centerPoint
  = let hexCoord = coord $ fromCenter centerPoint
        p1 = Point { coord = hexCoord, position = Top }
        p2 = Point { coord = hexCoord + (1, -1), position = Bottom }
        p3 = Point { coord = hexCoord + (0, 1), position = Top }
        p4 = Point { coord = hexCoord, position = Bottom }    
        p5 = Point { coord = hexCoord + (-1, 1), position = Top }
        p6 = Point { coord = hexCoord + (0, -1), position = Bottom }  
    in  [p1, p2, p3, p4, p5, p6]

allPoints :: [Point]
allPoints = nub (hexCenterPoints ++ concatMap (pointsAroundHex . toCenter) hexCenterPoints)

mkHexPointsAndEdges :: CentralPoint -> ([Point], [UndirectedEdge])
mkHexPointsAndEdges centerPoint
  = let edgePoints = pointsAroundHex centerPoint
        localPoints = (fromCenter centerPoint):edgePoints
        toCenters = map dupleToEdge $ zip (repeat $ fromCenter centerPoint) edgePoints
        loop = windowed 2 (edgePoints ++ [head edgePoints])
        loop' = map (dupleToEdge . fromJust . listToDuple) loop
        localEdges = loop' ++ toCenters
    in (localPoints, localEdges)

data EdgeType 
  = ToCenter        -- ^ Edge linking intersection with center (don't display)
  | BetweenCenters  -- ^ Between two centers (don't display)
  | Between         -- ^ Between two intersections
  deriving (Eq, Ord, Show, Read,Generic)

data UndirectedEdge = UndirectedEdge
  { point1 :: Point
  , point2 :: Point
  } deriving (Ord, Show, Read,Generic)

instance Eq UndirectedEdge where
  x == y = (point1 x == point1 y && point2 x == point2 y) || (point1 x == point2 y && point2 x == point1 y) 

dupleToEdge :: (Point, Point) -> UndirectedEdge
dupleToEdge (x, y) = UndirectedEdge x y

edgeType :: UndirectedEdge -> EdgeType
edgeType e | all (== Center) positions = BetweenCenters
           | any (== Center) positions && any (/= Center) positions = ToCenter
           | otherwise = Between
    where positions = map position [point1 e, point2 e]