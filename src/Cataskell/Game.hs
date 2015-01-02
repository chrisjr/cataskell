{-# LANGUAGE DeriveGeneric #-}
module Cataskell.Game where

import Control.Monad.Random
import System.Random.Shuffle
import Data.Maybe (listToMaybe)
import Cataskell.GameData.Actions
import Cataskell.GameData.Basics
import Cataskell.GameData.Board
import Cataskell.GameData.Player
import GHC.Generics (Generic)

data Phase = Initial | Normal | End
  deriving (Eq, Ord, Show, Read, Generic)

data GameState = GameState
  { phase :: Phase
  , board :: Board
  , players :: [Player]
  , currentPlayer :: Int
  , turnAdvanceBy :: Int
  , validActions :: [PlayerAction]
  } deriving (Eq, Ord, Show, Read,Generic)

-- | Makes a game from a list of player names
newGame :: (RandomGen g) => [String] -> Rand g GameState
newGame pNames = do
  b <- newBoard
  ps <- shuffleM $ mkPlayers pNames
  return $ GameState { phase = Initial 
                     , board = b
                     , players = ps
                     , currentPlayer = 0
                     , turnAdvanceBy = 1
                     , validActions = [(rollFor $ head ps)] }

getPlayer :: Color -> GameState -> Maybe Player
getPlayer c gs = listToMaybe . filter (\p -> color p == c) $ players gs