{-# LANGUAGE DeriveGeneric #-}

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
, checkNeighbors
, Board(..)
, newHexMap
, emptyBuildingMap
, emptyRoadMap
, newBoard
) where

import Cataskell.GameData.Basics
import Cataskell.GameData.Location
import Cataskell.GameData.Resources
import GHC.Generics (Generic)
import qualified Data.Map.Strict as Map
import Control.Exception (assert)
import Control.Monad.Random
import System.Random.Shuffle

data HexCenter = HexCenter
 { terrain :: Terrain
 , resource :: ResourceCount
 , roll :: Int
 , hasRobber :: Bool
 } deriving (Eq, Ord, Show, Generic)

type HexMap = Map.Map Point HexCenter
type RoadMap = Map.Map UndirectedEdge ActualRoad
type BuildingMap = Map.Map Point ActualBuilding

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
  } deriving (Eq, Show, Generic)

terrains :: [Terrain]
terrains = hills ++ pastures ++ mountains ++ fields ++ forests
  where hills = replicate 3 Hill
        pastures = replicate 4 Pasture
        mountains = replicate 3 Mountain
        fields = replicate 4 Field
        forests = replicate 4 Forest

rolls :: [Int]
rolls = [2, 12] ++ [3..6] ++ [3..6] ++ [8..11] ++ [8..11]

checkNeighbors :: HexMap -> Bool
checkNeighbors = undefined

newHexMap :: (RandomGen g) => Rand g HexMap
newHexMap = do
  terrains' <- shuffleM terrains
  rolls' <- shuffleM rolls
  desertLocation <- getRandomR (0, length terrains' - 1)
  let hexList = map (uncurry mkHexCenter) $ zip terrains' rolls'
  let (start, end) = splitAt desertLocation hexList
  let hexList' = start ++ [desert] ++ end
  return $ hexMapFromList hexList'

emptyBuildingMap :: BuildingMap
emptyBuildingMap = undefined

emptyRoadMap :: RoadMap
emptyRoadMap = undefined

newBoard :: (RandomGen g) => Rand g Board
newBoard = do
  hexMap <- newHexMap
  return Board { hexes = hexMap
               , roads = emptyRoadMap
               , buildings = emptyBuildingMap }