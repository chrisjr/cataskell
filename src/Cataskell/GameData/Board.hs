{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Cataskell.GameData.Board 
( HexCenter
, HexMap
, BuildingMap
, RoadMap
, terrain
, resource
, roll
, hasRobber
, mkHexCenter
, hexMapFromList
, terrains
, rolls
, checkHexNeighbors
, Board(..)
, newHexMap
, emptyBuildingMap
, emptyRoadMap
, newBoard
, freePoints
, build
, resourcesFromRoll
) where


import Cataskell.BoardGraph (allEdges, neighborPoints, resourceConnections, roadConnections)
import Cataskell.GameData.Basics
import Cataskell.GameData.Location
import Cataskell.GameData.Resources
import GHC.Generics (Generic)
import Data.List ((\\))
import Data.Monoid
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, isJust, isNothing)
import Control.Exception (assert)
import Control.Monad.Random
import System.Random.Shuffle

data HexCenter = HexCenter
 { terrain :: Terrain
 , resource :: ResourceCount
 , roll :: Int
 , hasRobber :: Bool
 } deriving (Eq, Ord, Show, Read,Generic)

type HexMap = Map.Map CentralPoint HexCenter
type RoadMap = Map.Map UndirectedEdge (Maybe ActualRoad)
type BuildingMap = Map.Map Point (Maybe ActualBuilding)

mkHexCenterUnsafe :: Terrain -> Int -> HexCenter
mkHexCenterUnsafe t r = HexCenter { terrain = t
                                  , roll = r
                                  , resource = resourceFromTerrain t
                                  , hasRobber = t == Desert }

-- | Make a hex with appropriate roll and terrain
mkHexCenter :: Terrain -> Int -> HexCenter
mkHexCenter t r = assert ((t == Desert) == (r == 7)) $ mkHexCenterUnsafe t r

desert :: HexCenter
desert = mkHexCenter Desert 7

hexMapFromList :: [HexCenter] -> HexMap
hexMapFromList = Map.fromList . zip hexCenterPoints

data Board = Board
  { hexes :: HexMap
  , roads :: RoadMap
  , buildings :: BuildingMap
  } deriving (Eq, Ord, Show, Read,Generic)

terrains :: [Terrain]
terrains = hills ++ pastures ++ mountains ++ fields ++ forests
  where hills = replicate 3 Hill
        pastures = replicate 4 Pasture
        mountains = replicate 3 Mountain
        fields = replicate 4 Field
        forests = replicate 4 Forest

rolls :: [Int]
rolls = [2, 12] ++ [3..6] ++ [3..6] ++ [8..11] ++ [8..11]

problemNeighborhoods :: HexMap -> ([CentralPoint], [CentralPoint])
problemNeighborhoods m
  = let highValued x = x == 6 || x == 8
        pointAndNeighborValued (p, ns) = highValued ((Map.!) rollsMap p) && any highValued ns
        lowValued (p, ns) = not ((highValued ((Map.!) rollsMap p)) || any highValued ns)
        rollsMap = Map.map roll m
        hn' = Map.map (catMaybes . map ((flip Map.lookup) rollsMap)) hexNeighborhoods
        lst = Map.toList hn'
    in  (map fst (filter pointAndNeighborValued lst), map fst (filter lowValued lst))

checkHexNeighbors :: HexMap -> Bool
checkHexNeighbors m
  = length (fst $ problemNeighborhoods m) == 0

newHexMap' :: (RandomGen g) => Rand g HexMap
newHexMap' = do
  terrains' <- shuffleM terrains
  rolls' <- shuffleM rolls
  desertLocation <- getRandomR (0, length terrains' - 1)
  let hexList = map (uncurry mkHexCenter) $ zip terrains' rolls'
  let (start, end) = splitAt desertLocation hexList
  let hexList' = start ++ [desert] ++ end
  return $ hexMapFromList hexList'

-- | If checkHexNeighbors fails, then swap one high-valued
swapHighValued :: HexMap -> HexMap
swapHighValued hexMap
  = let doSwap k1 k2 m = let v1 = (Map.!) m k1
                             v2 = (Map.!) m k2
                         in  Map.insert k2 v1 $ Map.insert k1 v2 m
        (tooHigh, tooLow) = problemNeighborhoods hexMap
        m' = if (length tooHigh == 0)
             then hexMap
             else doSwap (head tooHigh) (head tooLow) hexMap
    in  if (length tooHigh == 0) then m' else swapHighValued m'

-- | Recursively generates hexmaps until a valid one is found
newHexMap :: (RandomGen g) => Rand g HexMap
newHexMap = do
  m <- newHexMap'
  return $ if checkHexNeighbors m then m else swapHighValued m

emptyBuildingMap :: BuildingMap
emptyBuildingMap
  = let validPoints = filter (\x -> position x == Top || position x == Bottom) allPoints
    in Map.fromList $ zip validPoints (repeat Nothing)

emptyRoadMap :: RoadMap
emptyRoadMap
  = let validEdges = allEdges
    in Map.fromList $ zip validEdges (repeat Nothing)

newBoard :: (RandomGen g) => Rand g Board
newBoard = do
  hexMap <- newHexMap
  return Board { hexes = hexMap
               , roads = emptyRoadMap
               , buildings = emptyBuildingMap }

getHabitations :: Board -> Map.Map Point ActualBuilding
getHabitations b
  = Map.mapMaybe id $ buildings b

freePoints :: Board -> [Point]
freePoints b
  = let occupiedPoints = Map.keys . Map.filter (isJust) $ buildings b
        nns = concatMap (neighborPoints roadConnections) occupiedPoints
    in  allPoints \\ (occupiedPoints ++ nns)

-- | Adjusts the board to add a building/road.
-- | Checks that nothing was present (if settlement or road), or that a settlement
-- | belonging to the player was there (if city). 
build :: ActualBuilding -> Board -> Board
build bldg brd
  = case bldg of
      OnPoint (H Settlement p _) -> 
        let buildings' = Map.adjust (\x -> if isNothing x then (Just bldg) else x) p (buildings brd) 
        in brd { buildings = buildings'}
      OnPoint (H City p c) -> 
        let adjuster x = if x == (Just (OnPoint (H Settlement p c))) then (Just bldg) else x
            buildings' = Map.adjust adjuster p (buildings brd) 
        in brd { buildings = buildings'}
      OnEdge r@(R Road e _) -> 
        let roads' = Map.adjust (\x -> if isNothing x then (Just r) else x) e (roads brd) 
        in brd { roads = roads'}    

resourcesFromRoll :: Board -> Int -> Color -> ResourceCount
resourcesFromRoll b r c = foldl ((<>)) mempty lst
  where lst = map (\(b', r') -> if (habitationType b' == City) then mulResources r' 2 else r') bldgsRes
        bldgsRes = Map.elems $ Map.intersectionWith (\bld hex -> (bld, getResForHex hex)) bldgs ptToHex
        getResForHex hC = let res = resource hC
                         in  if hasRobber hC then mempty else res
        hexes' = Map.toList . Map.filter (\x -> roll x == r) $ hexes b
        mkPtsToHex (k, v) = zip (neighborPoints resourceConnections $ fromCenter k) (repeat v)
        ptToHex = Map.fromList $ concatMap mkPtsToHex hexes'
        bldgs = Map.filter (\x -> color x == c) $ getHabitations b
