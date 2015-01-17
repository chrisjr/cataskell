{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Cataskell.GameData.Board where

import Cataskell.BoardGraph (allEdges, getNodeMaybe, BoardGraph, neighborPoints, pathTree, resourceConnections, roadGraph, roadConnections)
import Cataskell.GameData.Basics
import Cataskell.GameData.Location
import Cataskell.GameData.Resources
import GHC.Generics (Generic)
import Control.Lens
import Control.Monad (join, liftM2)
import Data.Graph.Inductive
import Data.Graph.Inductive.PatriciaTree()
import Data.Monoid
import qualified Data.Map.Strict as Map
import Data.List (maximumBy, elemIndex)
import Data.Ord (comparing)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tree (Tree)
import qualified Data.Tree as Tree (levels, drawTree, drawForest)
import Data.Maybe (mapMaybe, listToMaybe, isNothing, fromJust, fromMaybe)
import Control.Exception (assert)
import Control.Monad.Random
import System.Random.Shuffle

data HexCenter = HexCenter
 { _terrain :: Terrain
 , _resource :: ResourceCount
 , _roll :: Int
 , _hasRobber :: Bool
 } deriving (Eq, Ord, Show, Read, Generic)

makeLenses ''HexCenter

type HexMap = Map.Map CentralPoint HexCenter
type RoadMap = Map.Map UndirectedEdge (Maybe OnEdge)
type BuildingMap = Map.Map Point (Maybe OnPoint)
type HarborMap = Map.Map Point Harbor

mkHexCenterUnsafe :: Terrain -> Int -> HexCenter
mkHexCenterUnsafe t r = HexCenter { _terrain = t
                                  , _roll = r
                                  , _resource = resourceFromTerrain t
                                  , _hasRobber = t == Desert }

-- | Make a hex with appropriate roll and terrain
mkHexCenter :: Terrain -> Int -> HexCenter
mkHexCenter t r = assert ((t == Desert) == (r == 7)) $ mkHexCenterUnsafe t r

desert :: HexCenter
desert = mkHexCenter Desert 7

hexMapFromList :: [HexCenter] -> HexMap
hexMapFromList = Map.fromList . zip hexCenterPoints

data Board = Board
  { _hexes :: HexMap
  , _roads :: RoadMap
  , _buildings :: BuildingMap
  , _harbors :: HarborMap
  } deriving (Eq, Ord, Show, Read, Generic)

makeLenses ''Board

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
        lowValued (p, ns) = not (highValued ((Map.!) rollsMap p) || any highValued ns)
        rollsMap = Map.map _roll m
        hn' = Map.map (mapMaybe (`Map.lookup` rollsMap)) hexNeighborhoods
        lst = Map.toList hn'
    in  (map fst (filter pointAndNeighborValued lst), map fst (filter lowValued lst))

checkHexNeighbors :: HexMap -> Bool
checkHexNeighbors m
  = null (fst $ problemNeighborhoods m)

newHexMap' :: (RandomGen g) => Rand g HexMap
newHexMap' = do
  terrains' <- shuffleM terrains
  rolls' <- shuffleM rolls
  desertLocation <- getRandomR (0, length terrains' - 1)
  let hexList = zipWith mkHexCenter terrains' rolls'
  let (start, end) = splitAt desertLocation hexList
  let hexList' = start ++ [desert] ++ end
  return $ hexMapFromList hexList'

-- | If two high-value hexes are next to each other, then swap one into a low-valued neighborhood
swapHighValued :: HexMap -> HexMap
swapHighValued hexMap
  = let doSwap k1 k2 m = let v1 = (Map.!) m k1
                             v2 = (Map.!) m k2
                         in  Map.insert k2 v1 $ Map.insert k1 v2 m
        (tooHigh, tooLow) = problemNeighborhoods hexMap
    in  if null tooHigh
        then hexMap
        else swapHighValued $ doSwap (head tooHigh) (head tooLow) hexMap

-- | Generates a hexmap and swaps high-value pieces until a valid one is found
newHexMap :: (RandomGen g) => Rand g HexMap
newHexMap = do
  m <- newHexMap'
  return $ if checkHexNeighbors m then m else swapHighValued m

harborPoints :: [[Point]]
harborPoints = [ [Point (-2,0) Bottom, Point (-3,2) Top]
               , [Point (-3,3) Top, Point (-2,2) Bottom]
               , [Point (-1,2) Bottom, Point (-1,3) Top]
               , [Point (1,1) Bottom, Point (1,2) Top]
               , [Point (3, -1) Bottom, Point (2,1) Top]
               , [Point (3, -2) Bottom, Point (2, -1) Top]
               , [Point (1, -2) Top, Point (2, -3) Bottom]
               , [Point (0,-2) Top, Point (0,-3) Bottom]
               ]

harborTypes :: [Harbor]
harborTypes = replicate 3 ThreeToOne ++ 
                [ Harbor Hill
                , Harbor Mountain
                , Harbor Forest
                , Harbor Pasture
                , Harbor Field
                ]

newHarborMap :: (RandomGen g) => Rand g HarborMap
newHarborMap = do
  harborPoints' <- shuffleM harborPoints
  harborTypes' <- shuffleM harborTypes
  let harborList = concatMap (\(ps,t) -> map (\p -> (p, t)) ps) $ zip harborPoints' harborTypes'
  return $ Map.fromList harborList

emptyBuildingMap :: BuildingMap
emptyBuildingMap
  = let validPoints = Set.filter (\x -> position x == Top || position x == Bottom) allPoints
    in Map.fromSet (const Nothing) validPoints

emptyRoadMap :: RoadMap
emptyRoadMap = Map.fromSet (const Nothing) allEdges

newBoard :: (RandomGen g) => Rand g Board
newBoard = do
  hexMap <- newHexMap
  harborMap <- newHarborMap
  return Board { _hexes = hexMap
               , _roads = emptyRoadMap
               , _buildings = emptyBuildingMap
               , _harbors = harborMap }

getHabitations :: Board -> Map.Map Point OnPoint
getHabitations b = Map.mapMaybe id $ view buildings b

filterByColor :: (Colored v) => Color -> Map.Map k v -> Map.Map k v
filterByColor color' = Map.filter ((== color') . color) 

getHabitationsFor :: Color -> Board -> Map.Map Point OnPoint
getHabitationsFor color' = filterByColor color' . getHabitations

getRoads :: Board -> Map.Map UndirectedEdge OnEdge
getRoads b = Map.mapMaybe id $ view roads b

getRoadsFor :: Color -> Board -> Map.Map UndirectedEdge OnEdge
getRoadsFor color' = filterByColor color' . getRoads

freePoints :: Board -> Set Point
freePoints b
  = let occupiedPoints = Map.keysSet $ getHabitations b
        nns = Set.unions $ map (neighborPoints roadConnections) (Set.toList occupiedPoints)
    in  (allPoints Set.\\ occupiedPoints) Set.\\ nns

freeEdges :: Board -> Set UndirectedEdge
freeEdges b
  = let occupiedEdges = Map.keysSet $ getRoads b
    in  allEdges Set.\\ occupiedEdges

validConstruct :: Construct -> Board -> Bool
validConstruct bldg brd
  = case bldg of
      Edifice (OnPoint p _ Settlement) ->
        p `Map.notMember` getHabitations brd
      Edifice (OnPoint p c City) ->
        Map.lookup p (getHabitations brd) == Just (OnPoint p c Settlement)
      Roadway (OnEdge e _) ->
        e `Map.notMember` getRoads brd

-- | Adjusts the board to add a building/road.
-- | Checks that nothing was present (if settlement or road), or that a settlement
-- | belonging to the player was there (if city). 
build :: Construct -> Board -> Board
build bldg brd
  = case bldg of
      Edifice bl@(OnPoint p _ Settlement) -> 
        let buildings' = Map.adjust (\x -> if isNothing x then Just bl else x) p (brd ^. buildings) 
        in buildings .~ buildings' $ brd
      Edifice bl@(OnPoint p c City) -> 
        let adjuster x = if x == Just (OnPoint p c Settlement) then Just bl else x
            buildings' = Map.adjust adjuster p (brd ^. buildings) 
        in buildings .~ buildings' $ brd
      Roadway bl@(OnEdge e _) -> 
        let roads' = Map.adjust (\x -> if isNothing x then Just bl else x) e (brd ^. roads)
        in roads .~ roads' $ brd

centersToNeighbors :: Map.Map CentralPoint [Point]
centersToNeighbors = Map.fromList $ zip hexCenterPoints (map pointsAroundHex hexCenterPoints)

getPointsToHexCentersMap :: Board -> Map.Map Point [HexCenter]
getPointsToHexCentersMap b = ptsToHexes
  where ptsToHexes = Map.unionsWith (++) $ concatMap mkPtToHCs hexes'
        mkPtToHCs (k, hC') = map (\x -> Map.singleton x [hC']) $ around k
        around = Set.toList . neighborPoints resourceConnections
        hexes' = Map.toList . Map.mapKeys fromCenter $ b ^. hexes

allResourcesFromRoll :: Int -> Board -> Map.Map Color ResourceCount
allResourcesFromRoll r b = colorSums
  where colorSums = foldl (Map.unionWith (<>)) (Map.empty :: Map.Map Color ResourceCount) maps
        maps = map (uncurry Map.singleton) lst
        lst = map (\(b', r') -> let r'' = if b'^.buildingType == City then mulResources r' 2 else r'
                                in (color b', r'')) bldgsRes
        bldgsRes = Map.elems $ Map.intersectionWith addUpRes bldgs ptsToHex
        addUpRes bld nhs = (bld, foldl (<>) mempty $ map getResForHex nhs)
        getResForHex hC = let res = _resource hC
                          in  if _hasRobber hC then mempty else res
        ptsToHex = Map.filter ((> 0) . length) rollRelevant
        rollRelevant = Map.map (filter ((== r) . _roll)) $ getPointsToHexCentersMap b
        bldgs = getHabitations b

allSurroundingHexes :: OnPoint -> Board -> [HexCenter]
allSurroundingHexes op' b = ptsToHex Map.! bldgPoint
  where ptsToHex = getPointsToHexCentersMap b
        bldgPoint = op'^.point

allStartingResources :: OnPoint -> Board -> ResourceCount
allStartingResources op' b = foldl (<>) mempty (map (view resource) hexes')
  where hexes' = allSurroundingHexes op' b

roadsToPointsFor :: Color -> Board -> Set Point
roadsToPointsFor color' board'
  = let myRoads = getRoadsFor color' board'
    in  Set.fromList . concatMap (\e -> [point1 e, point2 e]) $ Map.keys myRoads

enemyPoints :: Color -> Board -> Set Point
enemyPoints color' board' = Map.keysSet $ Map.filter ((/= color') . color) $ getHabitations board'

-- | A road can be built at the end of another road or a settlement, anywhere there isn't already one
validRoadsFor :: Color -> Board -> Set Construct
validRoadsFor color' board'
  = let myPoints = roadsToPointsFor color' board'
        enemyPoints' = enemyPoints color' board'
        freeEdges' = freeEdges board'
        pointsAdjacent e = filter (`Set.member` myPoints) [point1 e, point2 e]
        notEnemy ps | length ps == 1 = head ps `Set.notMember` enemyPoints' -- if only one endpoint belongs to me, can't build
                    | length ps == 2 = True -- if both endpoints belong to me, I can build even though the enemy is there
                    | otherwise = False
        validEdges = Set.filter (notEnemy . pointsAdjacent) freeEdges'
    in Set.map (\e -> built (road $ Just (e, color'))) validEdges

validSettlementsFor :: Color -> Board -> Set Construct
validSettlementsFor color' board'
  = let myPoints = roadsToPointsFor color' board'
        freePoints' = freePoints board'
        validPoints = Set.filter (`Set.member` myPoints) freePoints'
    in Set.map (\p -> built (settlement $ Just (p, color'))) validPoints

validCitiesFor :: Color -> Board -> Set Construct
validCitiesFor color' board'
  = let bldgs = getHabitationsFor color' board'
        points = Map.keysSet $ Map.filter (isSettlement . Building . Edifice) bldgs
    in  Set.map (\p -> built (city $ Just (p, color'))) points

roadGraphForColor :: Color -> Board -> Gr UndirectedEdge (UndirectedEdge, UndirectedEdge)
roadGraphForColor color' board'
  = let roads' = getRoadsFor color' board'
        gr' = roadGraph (Map.keysSet roads') (enemyPoints color' board')
    in undir gr'

maybeLongest :: Gr UndirectedEdge (UndirectedEdge, UndirectedEdge) -> Maybe Int
maybeLongest gr = do
  let depth s = map snd $ level s gr
  let depths = concatMap depth (nodes gr)
  if not (null depths)
    then Just (1 + maximum depths)
    else Nothing

longestRoadForColor :: Board -> Color -> (Color, Int)
longestRoadForColor board' color'
  = let gr = roadGraphForColor color' board'
        pathLength = maybeLongest gr
    in (color', fromMaybe 0 pathLength)

longestRoad :: Board -> (Color, Int)
longestRoad board'
  = maximumBy (comparing snd) $ map (longestRoadForColor board') [Red, White, Orange, Blue]
