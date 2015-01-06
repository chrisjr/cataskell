{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Cataskell.Game where

import Control.Monad.Identity
import Control.Monad.Random
import Control.Monad.State
import System.Random.Shuffle
import qualified Data.Map.Strict as Map
import Control.Exception (assert)
import Data.Maybe (fromJust, isJust)
import Data.List (findIndex, elemIndex)
import Cataskell.GameData.Actions
import Cataskell.GameData.Basics
import Cataskell.GameData.Board
import Cataskell.GameData.Player
import Cataskell.GameData.Resources
import Control.Lens
import GHC.Generics (Generic)

type GameState g = StateT Game (RandT g Identity) ()
type GameStateReturning g = StateT Game (RandT g Identity)

data Phase = Initial | Normal | RobberAttack | MovingRobber | FreeRoads | End
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

findPlayerByColor :: (RandomGen g) => Color -> GameStateReturning g Int
findPlayerByColor c = do
  maybeI <- uses players $ findIndex ((== c) . color)
  return $ fromJust maybeI

update :: (RandomGen g) => GameAction -> GameState g
update action' = do
  done <- checkAndExecute action'
  when (done && isJust (action' ^? action.construct.onEdge)) progress

checkAndExecute :: (RandomGen g) => GameAction -> GameStateReturning g Bool
checkAndExecute action' = do
  valid <- isValid action'
  when (valid) $ doAction action'
  return valid

isValid :: (RandomGen g) => GameAction -> GameStateReturning g Bool
isValid action' = do
  current <- get
  let predicates' = preconditions action'
  let valid = and $ map ($ current) predicates'
  return valid

phaseOneOf :: [Phase] -> (Game -> Bool)
phaseOneOf phases = (flip elem) phases . view phase

hasItem :: Item -> Player -> (Game -> Bool)
hasItem itemToFind player' = elem itemToFind . playerItems
  where playerItems = view (players.(ix (player'^.playerIndex)).constructed)

hasResourcesFor :: Item -> Player -> (Game -> Bool)
hasResourcesFor itemToBuy player' = (flip sufficient) cost' . playerResources
  where cost' = cost itemToBuy
        playerResources = view (players.(ix (player'^.playerIndex)).resources)

-- | Returns a series of predicates that must be true for an action to proceed
preconditions :: GameAction -> [Game -> Bool]
preconditions (PlayerAction player' action')
  = case action' of
      Roll -> []
      BuildForFree _ -> [phaseOneOf [Initial, FreeRoads]]
      PlayCard x -> [phaseOneOf [Normal],
                     hasItem (Card x) player']
      Purchase x -> [phaseOneOf [Normal],
                     hasResourcesFor x player']
      Trade _ -> []
      Discard _ -> []
      EndTurn -> []

doAction :: (RandomGen g) => GameAction -> GameState g
doAction act'
  = let action' = act' ^. action
        actor'  = act' ^. actor
    in  case action' of
          Roll -> doRoll
          BuildForFree c' -> do
            doBuild actor' c'
            let totalSettlements = length . filter isSettlement $ actor' ^. constructed
            if (deconstruct c' == Potential (H Settlement))
            then do
              when (totalSettlements == 2) $ giveStartingResources actor'
              let c'' = fromJust $ c' ^? onPoint
              newRoads <- uses (board.roads) (initialRoadsFor actor' c'')
              validActions .= newRoads
            else return ()
          Purchase x -> doPurchase actor' x
          Trade x -> doTrade actor' x
          Discard x -> doDiscard actor' x
          PlayCard x -> doPlayCard actor' x
          EndTurn -> progress


giveStartingResources :: (RandomGen g) => Player -> GameState g
giveStartingResources player' = do
  let pI = player' ^. playerIndex
  hasBuilt <- use (players . ix pI . constructed)
  let lastSettlement = last . filter isSettlement $ hasBuilt
  let p' = fromJust $ lastSettlement ^? building.onPoint
  b <- use board
  let res = allStartingResources p' b
  addToResources pI res

-- | Find an appropriate construct in the player inventory and, if it exists,
-- | execute the build request; otherwise do nothing.
doBuild :: (RandomGen g) => Player -> Construct -> GameState g
doBuild p' construct' = do
  let potential = deconstruct construct'
  b <- use board
  replaceInventory p' potential (Just $ Building construct') $ do
    board .= build construct' b

replaceInventory :: (RandomGen g) => Player -> Item -> Maybe Item -> GameState g -> GameState g
replaceInventory p' old' new' extraActions = do
  hasLeft <- use (players . ix (p' ^. playerIndex) . constructed)
  let i = elemIndex old' hasLeft
  if (isJust i)
  then do
    let i' = fromJust i
    if (isJust new')
    then do
      players . ix (p' ^. playerIndex) . constructed . ix i' .= fromJust new'
    else do
      players . ix (p' ^. playerIndex) . constructed .= hasLeft ^.. folded . ifiltered (\i'' _ -> i'' /= i')
    extraActions
  else return ()

addToResources :: (RandomGen g) => Int -> ResourceCount -> GameState g
addToResources i res = players . ix i . resources <>= res

doPurchase :: (RandomGen g) => Player -> Item -> GameState g
doPurchase = assert False undefined

doTrade :: (RandomGen g) => Player -> TradeAction -> GameState g
doTrade = assert False undefined

doDiscard :: (RandomGen g) => Player -> DiscardAction -> GameState g
doDiscard = assert False undefined

doPlayCard :: (RandomGen g) => Player -> DevelopmentCard -> GameState g
doPlayCard player' card' = do
  replaceInventory player' (Card card') Nothing $ do
    case card' of
      RoadBuilding -> toRoadBuilding player'
      Knight -> assert False undefined
      Invention -> assert False undefined
      Monopoly -> assert False undefined
      VictoryPoint -> return ()

toRoadBuilding :: (RandomGen g) => Player -> GameState g
toRoadBuilding player' = do
  phase .= FreeRoads
  possibleRoads <- uses board (validRoadsFor $ color player')
  validActions .= map (mkFree player') possibleRoads

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
  let newAdv x | x == 3 && adv == 1 = 0
               | x == 3 && adv == 0 = -1
               | otherwise = adv
  nextPlayer <- uses players ((flip (!!)) next')
  nextActionsIfInitial <- uses board (possibleInitialSettlements nextPlayer)
  case p of
    Initial -> if (current == 0 && adv == -1) 
               then initialToNormal
               else sequence_ $
                      [ currentPlayer .= next'
                      , validActions .= nextActionsIfInitial
                      , turnAdvanceBy .= newAdv next' ]
    Normal -> switchToPlayer next' [rollFor nextPlayer]
    RobberAttack -> return () -- if progress called when robber attacks, do nothing
    MovingRobber -> return () -- if progress called when moving robber, do nothing
    FreeRoads -> switchToPlayer next' [rollFor nextPlayer]
    End -> return ()

switchToPlayer :: (RandomGen g) => Int -> [GameAction] -> GameState g
switchToPlayer i valids = do
  currentPlayer .= i
  validActions .= valids

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
          i <- findPlayerByColor c
          addToResources i res)


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

-- | Choose a valid action at random (useful for testing purposes)
randomAct :: (RandomGen g) => GameState g
randomAct = do
  vA <- use validActions
  act' <- uniform vA
  update act'