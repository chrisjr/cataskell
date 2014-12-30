{-# LANGUAGE DeriveGeneric #-}

module Cataskell.GameData.Board where

import Cataskell.GameData.Basics
import Cataskell.GameData.Location
import Cataskell.GameData.Resources
import GHC.Generics (Generic)
import qualified Data.Map.Strict as Map
import Control.Monad.Random
import System.Random.Shuffle

data Hex = Hex
 { terrain :: Terrain
 , resource :: ResourceCount
 , roll :: Int
 , hasRobber :: Bool
 } deriving (Eq, Show, Generic)

type HexMap = Map.Map HexCoord Hex

mkHex :: Terrain -> Int -> Hex
mkHex t r 
  = Hex { terrain = t
        , roll = r
        , resource = resourceFromTerrain t
        , hasRobber = t == Desert }

hexMapFromList :: [Hex] -> HexMap
hexMapFromList = Map.fromList . zip hexCoords

data Board = Board
  { hexes :: HexMap
  } deriving (Eq, Show, Generic)

terrains :: [Terrain]
terrains = hills ++ pastures ++ mountains ++ fields ++ forests ++ [Desert]
  where hills = replicate 3 Hill
        pastures = replicate 4 Pasture
        mountains = replicate 3 Mountain
        fields = replicate 4 Field
        forests = replicate 4 Forest

rolls :: [Int]
rolls = [2, 7, 12] ++ [3..11] ++ [3..11]

newHexMap :: (RandomGen g) => Rand g HexMap
newHexMap = do
  terrains' <- shuffleM terrains
  rolls' <- shuffleM rolls
  let hexList = map (uncurry mkHex) $ zip terrains' rolls'
  return $ hexMapFromList hexList

newBoard :: (RandomGen g) => Rand g Board
newBoard = do
  hexMap <- newHexMap
  return Board { hexes = hexMap }


placements :: Board -> [Construct]
placements = undefined
