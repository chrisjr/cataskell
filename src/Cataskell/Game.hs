{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
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
import Control.Lens
import GHC.Generics (Generic)

type GameState g = StateT Game (RandT g Identity) ()

data Phase = Initial | Normal | RobberAttack | MovingRobber | End
  deriving (Eq, Ord, Show, Read, Generic)

data Game = Game
  { _phase :: Phase
  , _board :: Board
  , _players :: [Player]
  , _currentPlayer :: Int
  , _turnAdvanceBy :: Int
  , _rolled :: Maybe Int
  , _validActions :: [GameAction]
  , _winner :: Maybe Player
  } deriving (Eq, Ord, Show, Read, Generic)

makeLenses ''Game

-- | Makes a game from a list of player names
newGame :: (RandomGen g) => [String] -> Rand g Game
newGame pNames = do
  b <- newBoard
  ps <- shuffleM $ mkPlayers pNames
  return $ Game { _phase = Initial 
                , _board = b
                , _players = ps
                , _currentPlayer = 0
                , _turnAdvanceBy = 1
                , _rolled = Nothing
                , _validActions = possibleInitialSettlements (head ps) b
                , _winner = Nothing }

getPlayer :: Color -> Game -> Maybe Player
getPlayer c gs = listToMaybe . filter (\p -> color p == c) $ gs ^. players

validInContext :: GameAction -> Game -> Bool
validInContext action' game = case (game ^. phase) of
  Initial ->
    action' `elem` (game ^. validActions)
  Normal ->
    action' `elem` (game ^. validActions)
  RobberAttack ->
    action' `elem` (game ^. validActions)
  MovingRobber ->
    action' `elem` (game ^. validActions)
  End ->
    False

doRoll :: (RandomGen g) => GameState g
doRoll = do
  r <- getRandomR (1, 6)
  rolled .= (Just r)

update :: (RandomGen g) => GameAction -> GameState g
update action' = do
  p <- use phase
  case p of
    Initial ->
      updateInitial action'
    Normal ->
      undefined
    RobberAttack ->
      undefined
    MovingRobber ->
      undefined
    End ->
      undefined

updateInitial :: (RandomGen g) => GameAction -> GameState g
updateInitial = undefined

toNormal :: (RandomGen g) => GameState g
toNormal = do
  phase .= Normal
  p <- (uses players head)
  validActions .= [rollFor p]

wonBy :: (RandomGen g) => Player -> GameState g
wonBy p = do
  phase .= End
  winner .= Just p