{-# LANGUAGE DeriveGeneric #-}

module Cataskell.GameData.Board where

import Cataskell.GameData.Basics
import Cataskell.GameData.Resources
import Cataskell.Util (randomSt)
import GHC.Generics (Generic)
import System.Random
import Control.Monad.Trans.State.Strict

data Hex = Hex
 { terrain :: Terrain
 , resource :: ResourceCount
 , roll :: Int
 , hasRobber :: Bool
 } deriving (Eq, Show, Generic)

mkHexGraph :: Terrain -> Int -> Hex
mkHexGraph t r 
  = Hex { terrain = t
        , roll = r
        , resource = resourceFromTerrain t
        , hasRobber = t == Desert }

data Board = Board
  { hexes :: [Hex]
  }

mkBoard :: State StdGen Board
mkBoard = undefined

newBoard :: StdGen -> (Board, StdGen)
newBoard = runState mkBoard

placements :: Board -> [Construct]
placements = undefined
