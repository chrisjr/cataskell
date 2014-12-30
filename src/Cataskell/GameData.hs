{-# LANGUAGE DeriveGeneric #-}

module Cataskell.GameData where

import Cataskell.GameData.Basics
import Cataskell.GameData.Resources
-- import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)

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

data Player = Player
  { color :: Color
  , resources :: ResourceCount
  , constructed :: [Construct]
  , bonuses :: [Bonus]
  } deriving (Eq, Show, Generic)

getMaskedScore :: Player -> Int
getMaskedScore p = undefined

getScore :: Player -> Int
getScore p = regularPoints + bonusPoints
  where regularPoints = totalPointsOf $ constructed p
        bonusPoints   = totalPointsOf $ bonuses p
        totalPointsOf x = sum $ map pointValue x

data BoardData = BoardData
  {
  }

data GameState = GameState
  { players :: [Player]
  , currentPlayer :: Int
  , board :: BoardData
  }
