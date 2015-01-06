{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Cataskell.GameData.Board 
( HexCenter
, HexMap
, BuildingMap
, RoadMap
, HarborMap
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
, hexes
, roads
, buildings
, harbors
, harborPoints
, harborTypes
, harborDiscount
, genericHarborDiscount
, newHexMap
, newHarborMap
, emptyBuildingMap
, emptyRoadMap
, newBoard
, freePoints
, build
, allResourcesFromRoll
, allStartingResources
, validRoadsFor
) where


import Cataskell.BoardGraph (allEdges, neighborPoints, resourceConnections, roadConnections)
import Cataskell.GameData.Basics
import Cataskell.GameData.Location
import Cataskell.GameData.Resources
import GHC.Generics (Generic)
import Data.List ((\\))
import Control.Lens
import Data.Monoid
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, listToMaybe, isJust, isNothing)
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
type RoadMap = Map.Map UndirectedEdge (Maybe OnEdge)
type BuildingMap = Map.Map Point (Maybe OnPoint)
type HarborMap = Map.Map Point Harbor

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
  { _hexes :: HexMap
  , _roads :: RoadMap
  , _buildings :: BuildingMap
  , _harbors :: HarborMap
  } deriving (Eq, Ord, Show, Read,Generic)

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

genericHarborDiscount :: Int -> (ResourceCount -> Int)
genericHarborDiscount i
  = let f r = case r of
                ResourceCount a b c d e -> maximum [a,b,c,d,e] `div` i
    in f

harborDiscount :: Harbor -> (ResourceCount -> Int)
harborDiscount harbor'
  = let f = case harbor' of
              Harbor Hill -> \r -> brick r `div` 2
              Harbor Forest -> \r -> lumber r `div` 2
              Harbor Pasture -> \r -> wool r `div` 2
              Harbor Field -> \r -> wheat r `div` 2
              Harbor Mountain -> \r -> ore r `div` 2
              Harbor Desert -> \_ -> 0 -- TODO: should this exist?
              ThreeToOne -> genericHarborDiscount 3
    in  f

newHarborMap :: (RandomGen g) => Rand g HarborMap
newHarborMap = do
  harborPoints' <- shuffleM harborPoints
  harborTypes' <- shuffleM harborTypes
  let harborList = concatMap (\(ps,t) -> map (\p -> (p, t)) ps) $ zip harborPoints' harborTypes'
  return $ Map.fromList harborList

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
  harborMap <- newHarborMap
  return Board { _hexes = hexMap
               , _roads = emptyRoadMap
               , _buildings = emptyBuildingMap
               , _harbors = harborMap }

getHabitations :: Board -> Map.Map Point OnPoint
getHabitations b
  = Map.mapMaybe id $ view buildings b

freePoints :: Board -> [Point]
freePoints b
  = let occupiedPoints = Map.keys . Map.filter (isJust) $ view buildings b
        nns = concatMap (neighborPoints roadConnections) occupiedPoints
    in  allPoints \\ (occupiedPoints ++ nns)

-- | Adjusts the board to add a building/road.
-- | Checks that nothing was present (if settlement or road), or that a settlement
-- | belonging to the player was there (if city). 
build :: Construct -> Board -> Board
build bldg brd
  = case bldg of
      Edifice bl@(OnPoint p _ Settlement) -> 
        let buildings' = Map.adjust (\x -> if isNothing x then (Just bl) else x) p (brd ^. buildings) 
        in buildings .~ buildings' $ brd
      Edifice bl@(OnPoint p c City) -> 
        let adjuster x = if x == Just (OnPoint p c Settlement) then (Just bl) else x
            buildings' = Map.adjust adjuster p (brd ^. buildings) 
        in buildings .~ buildings' $ brd
      Roadway bl@(OnEdge e _) -> 
        let roads' = Map.adjust (\x -> if isNothing x then (Just bl) else x) e (brd ^. roads)
        in roads .~ roads' $ brd

getPointsToHexCentersMap :: Board -> Map.Map Point [HexCenter]
getPointsToHexCentersMap b = ptsToHexes
  where ptsToHexes = Map.unionsWith (++) $ concatMap mkPtToHCs hexes'
        mkPtToHCs (k, hC') = map (\x -> Map.singleton x [hC']) $ around k
        around = neighborPoints resourceConnections
        hexes' = Map.toList . Map.mapKeys fromCenter $ b ^. hexes

allResourcesFromRoll :: Int -> Board -> Map.Map Color ResourceCount
allResourcesFromRoll r b = colorSums
  where colorSums = foldl (Map.unionWith (<>)) (Map.empty :: Map.Map Color ResourceCount) maps
        maps = map (uncurry Map.singleton) lst
        lst = map (\(b', r') -> let r'' = if (b'^.buildingType == City) then mulResources r' 2 else r'
                                in (color b', r'')) bldgsRes
        bldgsRes = Map.elems $ Map.intersectionWith addUpRes bldgs ptsToHex
        addUpRes bld nhs = (bld, foldl (<>) mempty $ map getResForHex nhs)
        getResForHex hC = let res = resource hC
                          in  if hasRobber hC then mempty else res
        ptsToHex = Map.filter ((> 0) . length) rollRelevant
        rollRelevant = Map.map (filter ((== r) . roll)) $ getPointsToHexCentersMap b
        bldgs = getHabitations b

allStartingResources :: OnPoint -> Board -> ResourceCount
allStartingResources op' b = maybe mempty snd pointRes
  where pointRes = listToMaybe $ Map.toList bldgRes
        bldgRes = Map.intersectionWith (\_ hexes' -> foldl (<>) mempty $ map resource hexes') bldg ptsToHex
        ptsToHex = getPointsToHexCentersMap b
        bldg = Map.singleton (op'^.point) op'

validRoadsFor :: Color -> Board -> [Construct]
validRoadsFor = assert False undefined