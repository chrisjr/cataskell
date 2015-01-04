{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Cataskell.Game where

import Control.Monad.Identity
import Control.Monad.Random
import Control.Monad.State
import System.Random.Shuffle
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.List (findIndex)
import Cataskell.GameData.Actions
import Cataskell.GameData.Basics
import Cataskell.GameData.Board
import Cataskell.GameData.Player
import Cataskell.GameData.Resources
import Control.Lens
import GHC.Generics (Generic)

type GameState g = StateT Game (RandT g Identity) ()
type GameStateReturning g = StateT Game (RandT g Identity)

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

runGame :: (RandomGen g) => GameState g -> Game -> g -> Game
runGame stModify game stdgen = evalRand (execStateT stModify game) stdgen

findPlayer :: (RandomGen g) => Color -> GameStateReturning g Int
findPlayer c = do
  maybeI <- uses players $ findIndex ((== c) . color)
  return $ fromJust maybeI

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
updateInitial action' = do
  totalPlayers <- uses players length
  let lastPlayerIndex = totalPlayers - 1
  currentIndex <- use currentPlayer

  when (lastPlayerIndex == currentIndex) $ turnAdvanceBy .= -1

  checkAndExecute action'

checkAndExecute :: (RandomGen g) => GameAction -> GameState g
checkAndExecute action' = do
  valid <- isValid action'
  when (valid) $ doAction action'

isValid :: (RandomGen g) => GameAction -> GameStateReturning g Bool
isValid action' = do
  current <- get
  let predicates' = preconditions action'
  let valid = and $ map ($ current) predicates'
  return valid

-- | Returns a series of predicates that must be true for an action to proceed
preconditions :: GameAction -> [Game -> Bool]
preconditions = undefined

doAction :: (RandomGen g) => GameAction -> GameState g
doAction = undefined

doRoll :: (RandomGen g) => GameState g
doRoll = do
  d1 <- getRandomR (1, 6)
  d2 <- getRandomR (1, 6)
  let r' = d1 + d2
  rolled .= (Just r')
  updateForRoll

-- | Called on EndTurn action
progress :: (RandomGen g) => GameState g
progress = do
  current <- use currentPlayer
  adv <- use turnAdvanceBy
  totalPlayers <- uses players length
  p <- use phase
  let next' = (totalPlayers + current + adv) `mod` totalPlayers 
  nextPlayer <- uses players ((flip (!!)) next')
  nextActionsIfInitial <- uses board (possibleInitialSettlements nextPlayer)
  case p of
    Initial -> sequence_ $
      [ currentPlayer .= next'
      , validActions .= nextActionsIfInitial ]
    Normal -> sequence_ $
      [ currentPlayer .= next'
      , validActions .= [rollFor nextPlayer]]
    RobberAttack -> return ()
    MovingRobber -> return ()
    End -> return ()
-- * State transitions

updateForRoll :: (RandomGen g) => GameState g
updateForRoll = do
  r' <- use rolled
  case r' of
    Just 7 -> toRobberAttack
    _ -> distributeResources

distributeResources :: (RandomGen g) => GameState g
distributeResources = do
  r' <- use rolled
  colorResUpdates <- uses board (allResourcesFromRoll $ fromJust r')
  forM_ (Map.toList colorResUpdates)
        (\(c, res) -> do
          i <- findPlayer c
          players . ix i . resources <>= res)


-- | Move to Normal phase (from Initial)
initialToNormal :: (RandomGen g) => GameState g
initialToNormal = do
  phase .= Normal
  p <- (uses players head)
  validActions .= [rollFor p]

-- | When a 7 is rolled, move into RobberAttack. If no one has more than 7 resources, move on to MovingRobber.
toRobberAttack :: (RandomGen g) => GameState g
toRobberAttack = do
  ps <- use players
  let pRes = map (\p -> (p, totalResources (p ^. resources))) ps
  let mustDiscard = map mkDiscard $ filter (\(_, r) -> r > 7) pRes
  if (length mustDiscard > 0) then sequence_ [ validActions .= mustDiscard
                                             , phase .= RobberAttack ]
                              else toMovingRobber

-- | Switch to MovingRobber phase
toMovingRobber :: (RandomGen g) => GameState g
toMovingRobber = do
  phase .= MovingRobber

-- | Game won by player p.
wonBy :: (RandomGen g) => Player -> GameState g
wonBy p = do
  phase .= End
  winner .= Just p