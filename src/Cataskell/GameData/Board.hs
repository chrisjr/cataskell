{-# LANGUAGE DeriveGeneric #-}

module Cataskell.GameData.Board where

import Cataskell.GameData.Basics
import Cataskell.GameData.Resources
import GHC.Generics (Generic)
import Control.Monad.Random

data Hex = Hex
 { terrain :: Terrain
 , resource :: ResourceCount
 , roll :: Int
 , hasRobber :: Bool
 } deriving (Eq, Show, Generic)

mkHex :: Terrain -> Int -> Hex
mkHex t r 
  = Hex { terrain = t
        , roll = r
        , resource = resourceFromTerrain t
        , hasRobber = t == Desert }

data Board = Board
  { hexes :: [Hex]
  }

newBoard :: (RandomGen g) => Rand g Board
newBoard = undefined

placements :: Board -> [Construct]
placements = undefined
