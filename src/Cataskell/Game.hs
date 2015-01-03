{-# LANGUAGE DeriveGeneric #-}
module Cataskell.Game where

import Control.Monad.Identity
import Control.Monad.Random
import Control.Monad.State
import System.Random.Shuffle
import Data.Maybe (listToMaybe)
import Cataskell.GameData.Actions
import Cataskell.GameData.Basics
import Cataskell.GameData.Board
import Cataskell.GameData.Player
import GHC.Generics (Generic)

type GameState g = StateT Game (RandT g Identity) ()

data Phase = Initial | Normal | RobberAttack | MovingRobber | End
  deriving (Eq, Ord, Show, Read, Generic)

data Game = Game
  { phase :: Phase
  , board :: Board
  , players :: [Player]
  , currentPlayer :: Int
  , turnAdvanceBy :: Int
  , rolled :: Maybe Int
  , validActions :: [PlayerAction]
  , winner :: Maybe Player
  } deriving (Eq, Ord, Show, Read,Generic)

-- | Makes a game from a list of player names
newGame :: (RandomGen g) => [String] -> Rand g Game
newGame pNames = do
  b <- newBoard
  ps <- shuffleM $ mkPlayers pNames
  return $ Game { phase = Initial 
                , board = b
                , players = ps
                , currentPlayer = 0
                , turnAdvanceBy = 1
                , rolled = Nothing
                , validActions = possibleInitialSettlements (head ps) b
                , winner = Nothing }

getPlayer :: Color -> Game -> Maybe Player
getPlayer c gs = listToMaybe . filter (\p -> color p == c) $ players gs

validInContext :: PlayerAction -> Game -> Bool
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

ex1 :: (RandomGen g) => GameState g
ex1 = do
  r <- getRandomR (1, 6)
  s <- get
  put s { rolled = (Just r) }

doRoll :: (RandomGen g) => Game -> Rand g Game
doRoll x = do
  r' <- getRandomR (1, 6)
  return $ x { rolled = (Just r') }

update :: PlayerAction -> Game -> Game
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

toEnd :: Player -> Game -> Game
toEnd p game = game { phase = End, winner = Just p }