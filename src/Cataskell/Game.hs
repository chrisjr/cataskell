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

data Phase = Initial | Normal | RobberAttack | MovingRobber | End
  deriving (Eq, Ord, Show, Read, Generic)

data GameState = GameState
  { phase :: Phase
  , board :: Board
  , players :: [Player]
  , currentPlayer :: Int
  , turnAdvanceBy :: Int
  , validActions :: [PlayerAction]
  , winner :: Maybe Player
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
                     , validActions = possibleInitialSettlements (head ps) b
                     , winner = Nothing }

getPlayer :: Color -> GameState -> Maybe Player
getPlayer c gs = listToMaybe . filter (\p -> color p == c) $ players gs

validInContext :: PlayerAction -> GameState -> Bool
validInContext pAction game = case (phase game) of
  Initial ->
    pAction `elem` validActions game
  Normal ->
    pAction `elem` validActions game
  RobberAttack ->
    pAction `elem` validActions game
  MovingRobber ->
    pAction `elem` validActions game
  End ->
    False

update :: PlayerAction -> GameState -> GameState
update pAction game = case (phase game) of
  Initial ->
    let isValid = validInContext pAction game
        game' = game
    in  if isValid then game' else game
  Normal ->
    undefined
  RobberAttack ->
    undefined
  MovingRobber ->
    undefined
  End ->
    undefined

toEnd :: Player -> GameState -> GameState
toEnd p game = game { phase = End, winner = Just p }