{-# LANGUAGE DeriveGeneric #-}

module Cataskell.GameData.Board 
( Hex
, terrain
, resource
, roll
, hasRobber
, mkHex
, hexMapFromList
, checkNeighbors
, Board(..)
, newHexMap
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

data Hex = Hex
 { terrain :: Terrain
 , resource :: ResourceCount
 , roll :: Int
 , hasRobber :: Bool
 } deriving (Eq, Show, Generic)

type HexMap = Map.Map HexCoord Hex

mkHexUnsafe :: Terrain -> Int -> Hex
mkHexUnsafe t r = Hex { terrain = t
                      , roll = r
                      , resource = resourceFromTerrain t
                      , hasRobber = t == Desert }

-- | Make a hex with appropriate roll and terrain
mkHex :: Terrain -> Int -> Hex
mkHex t r = assert ((t == Desert) == (r == 7)) $ mkHexUnsafe t r

desert :: Hex
desert = mkHex Desert 7

hexMapFromList :: [Hex] -> HexMap
hexMapFromList = Map.fromList . zip hexCoords

data Board = Board
  { hexes :: HexMap
  , placements :: [Construct]
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
  let hexList = map (uncurry mkHex) $ zip terrains' rolls'
  let (start, end) = splitAt desertLocation hexList
  let hexList' = start ++ [desert] ++ end
  return $ hexMapFromList hexList'

newBoard :: (RandomGen g) => Rand g Board
newBoard = do
  hexMap <- newHexMap
  return Board { hexes = hexMap
               , placements = [] }