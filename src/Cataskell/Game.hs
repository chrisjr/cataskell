{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Cataskell.Game where

import Control.Monad.Identity
import Control.Monad.Random
import Control.Monad.State
import System.Random.Shuffle
import qualified Data.Map.Strict as Map
import Control.Exception (assert)
import Data.Monoid (mempty)
import Data.Maybe (fromJust, isJust, catMaybes)
import Data.List (findIndex, elemIndex)
import Cataskell.GameData.Actions
import Cataskell.GameData.Basics
import Cataskell.GameData.Board
import Cataskell.GameData.Location
import Cataskell.GameData.Player
import Cataskell.GameData.Resources
import Control.Lens
import GHC.Generics (Generic)

type GameState g = StateT Game (RandT g Identity) ()
type GameStateReturning g = StateT Game (RandT g Identity)

-- | A game phase that allows only one type of action.
data SpecialPhase
  = RobberAttack
  | MovingRobber
  | FreeRoads Int
  | Inventing
  | Monopolizing
  deriving (Eq, Ord, Show, Read, Generic)

data Phase = Initial | Normal | Special SpecialPhase | End
  deriving (Eq, Ord, Show, Read, Generic)

data Game = Game
  { _phase :: Phase
  , _board :: Board
  , _players :: [Player]
  , _currentPlayer :: PlayerIndex
  , _turnAdvanceBy :: Int
  , _rolled :: Maybe Int
  , _validActions :: [GameAction]
  , _openTrades :: [TradeAction]
  , _allCards :: [Item]
  , _winner :: Maybe PlayerIndex
  } deriving (Eq, Ord, Show, Read, Generic)

makeLenses ''Game

-- | Makes a game from a list of player names
newGame :: (RandomGen g) => [String] -> Rand g Game
newGame pNames = do
  b <- newBoard
  shuffledNames <- shuffleM pNames
  cards <- shuffleM allDevelopmentCards
  let ps = mkPlayers shuffledNames
  return $ Game { _phase = Initial 
                , _board = b
                , _players = ps
                , _currentPlayer = toPlayerIndex 0
                , _turnAdvanceBy = 1
                , _rolled = Nothing
                , _validActions = possibleInitialSettlements (head ps) b
                , _openTrades = []
                , _allCards = cards
                , _winner = Nothing }

runGame :: (RandomGen g) => GameState g -> Game -> g -> (Game, g)
runGame stModify game stdgen = runRand (execStateT stModify game) stdgen

findPlayerByColor :: (RandomGen g) => Color -> GameStateReturning g PlayerIndex
findPlayerByColor c = do
  maybeI <- uses players $ findIndex ((== c) . color)
  return $ toPlayerIndex $ fromJust maybeI

-- | Move from one game state to the next
update :: (RandomGen g) => GameAction -> GameState g
update action' = do
  done <- checkAndExecute action'
  p <- use phase
  let builtFreeRoad = isJust (action' ^? action.construct.onEdge)
  let movedRobber = isJust (action' ^? action.specialAction.moveRobber)

  when (done && builtFreeRoad) $ do
    if (p == Initial) then progress
    else handleFreeRoads p
  when (done && p == Special MovingRobber && movedRobber) backToNormal

-- | Move from one game state to the next
handleFreeRoads :: (RandomGen g) => Phase -> GameState g
handleFreeRoads phase' = do
  playerIndex' <- use currentPlayer
  when (phase' == Special (FreeRoads 2)) $ toSpecialPhase (FreeRoads 1) playerIndex'
  when (phase' == Special (FreeRoads 1)) $ backToNormal
  return ()

-- | Checks that the action is valid and executes it if so
checkAndExecute :: (RandomGen g) => GameAction -> GameStateReturning g Bool
checkAndExecute action' = do
  valid <- isValid action'
  when (valid) $ doAction action'
  return valid

-- | Returns true iff all preconditions are met
isValid :: (RandomGen g) => GameAction -> GameStateReturning g Bool
isValid action' = do
  current <- get
  let predicates' = preconditions action'
  let valid = and $ map ($ current) predicates'
  return valid

-- | Creates a predicate that is true when a game is in one of the specified phases.
phaseOneOf :: [Phase] -> (Game -> Bool)
phaseOneOf phases = (flip elem) phases . view phase

-- | Creates a predicate that checks if a player has an item
hasItem :: Item -> PlayerIndex -> (Game -> Bool)
hasItem itemToFind pI = elem itemToFind . playerItems
  where playerItems = view (players.(ix $ fromPlayerIndex pI).constructed)

-- | Creates a predicate that checks if a player has enough resources for something.
hasResourcesFor :: ResourceCount -> PlayerIndex -> (Game -> Bool)
hasResourcesFor cost' pI = (flip sufficient) cost' . playerResources
  where playerResources = view (players.(ix $ fromPlayerIndex pI).resources)

-- | Creates a predicate that checks if a player has enough resources for an item.
hasResourcesForItem :: Item -> PlayerIndex -> (Game -> Bool)
hasResourcesForItem itemToBuy pI = hasResourcesFor (cost itemToBuy) pI

getPlayer :: (RandomGen g) => PlayerIndex -> GameStateReturning g Player
getPlayer pI = do
  current <- get
  return $ current ^?! players . (ix $ fromPlayerIndex pI)

scoreFor :: (RandomGen g) => PlayerIndex -> GameStateReturning g Int
scoreFor pI = do
  player' <- getPlayer pI
  return $ view score player'

-- | Returns a series of predicates that must be true for an action to proceed.
preconditions :: GameAction -> [Game -> Bool]
preconditions (PlayerAction playerIndex' action')
  = case action' of
      Roll -> [ phaseOneOf [Normal]
              , (\g -> view currentPlayer g == playerIndex')]
      BuildForFree _ -> [phaseOneOf [Initial, Special (FreeRoads 2), Special (FreeRoads 1)]]
      SpecialAction x -> case x of
        M _ -> [phaseOneOf [Special Monopolizing]]
        I _ -> [phaseOneOf [Special Inventing]]
        R _ -> [phaseOneOf [Special MovingRobber]]
      PlayCard x -> [ phaseOneOf [Normal]
                    , hasItem (Card x) playerIndex'
                    , (\_ -> x /= VictoryPoint)] -- can't play a victory point
      Purchase x -> [ phaseOneOf [Normal]
                    , hasResourcesForItem x playerIndex']
      Trade x -> case x of
        Offer offer' -> [phaseOneOf [Normal], hasResourcesFor (offer'^.offering) playerIndex']
        Accept offer' _ -> [phaseOneOf [Normal], hasResourcesFor (offer'^.asking) playerIndex']
        Reject _ _ _ -> [phaseOneOf [Normal]]
        CompleteTrade offer' accepterIndex' -> [ phaseOneOf [Normal]
                                          , hasResourcesFor (offer'^.offering) playerIndex'
                                          , hasResourcesFor (offer'^.asking) accepterIndex']
        CancelTrade _ -> [phaseOneOf [Normal]]
        Exchange offer' -> [phaseOneOf [Normal], hasResourcesFor (offer'^.offering) playerIndex']
      Discard (DiscardAction i r) -> [ phaseOneOf [Special RobberAttack]
                                     , (\_ -> totalResources r == i) ]
      EndTurn -> [phaseOneOf [Normal]]

doAction :: (RandomGen g) => GameAction -> GameState g
doAction act'
  = let action' = act' ^. action
        actorIndex  = act' ^. actor
    in  case action' of
          Roll -> doRoll
          BuildForFree c' -> do
            doBuild actorIndex c'
            player' <- getPlayer actorIndex
            let totalSettlements = length . filter isSettlement $ player' ^. constructed
            if (deconstruct c' == Potential (H Settlement))
            then do
              when (totalSettlements == 2) $ giveStartingResources actorIndex
              let c'' = fromJust $ c' ^? onPoint
              newRoads <- uses (board.roads) (initialRoadsFor player' c'')
              validActions .= newRoads
            else return ()
          SpecialAction x -> case x of
            M monopoly' -> doMonopoly actorIndex monopoly'
            I invention' -> doInvention actorIndex invention'
            R moveRobber' -> doMoveRobber actorIndex moveRobber'
          Purchase x -> doPurchase actorIndex x -- already checked resources
          Trade x -> doTrade actorIndex x
          Discard x -> doDiscard actorIndex x
          PlayCard x -> doPlayCard actorIndex x
          EndTurn -> do
            scoreIsNow <- scoreFor actorIndex
            if scoreIsNow >= 10
            then wonBy actorIndex
            else progress

giveStartingResources :: (RandomGen g) => PlayerIndex -> GameState g
giveStartingResources pI = do
  hasBuilt <- use (players . ix (fromPlayerIndex pI) . constructed)
  let lastSettlement = last . filter isSettlement $ hasBuilt
  let p' = fromJust $ lastSettlement ^? building.onPoint
  b <- use board
  let res = allStartingResources p' b
  addToResources pI res

-- | Find an appropriate construct in the player inventory and, if it exists,
-- | execute the build request; otherwise do nothing.
doBuild :: (RandomGen g) => PlayerIndex -> Construct -> GameState g
doBuild p' construct' = do
  let potential = deconstruct construct'
  b <- use board
  replaceInventory p' potential (Just $ Building construct') $ do
    board .= build construct' b

inventory :: (RandomGen g) => PlayerIndex -> GameStateReturning g [Item]
inventory playerIndex' = getPlayer playerIndex' >>= (return . view constructed)

replaceInventory :: (RandomGen g) => PlayerIndex -> Item -> Maybe Item -> GameState g -> GameState g
replaceInventory playerIndex' old' new' extraActions = do
  hasLeft <- use (players . ix (fromPlayerIndex playerIndex') . constructed)
  let i = elemIndex old' hasLeft
  if (isJust i)
  then do
    let i' = fromJust i
    if (isJust new')
    then do
      players . ix (fromPlayerIndex playerIndex') . constructed . ix i' .= fromJust new'
    else do
      players . ix (fromPlayerIndex playerIndex') . constructed .= hasLeft ^.. folded . ifiltered (\i'' _ -> i'' /= i')
    extraActions
  else return ()

addToResources :: (RandomGen g) => PlayerIndex -> ResourceCount -> GameState g
addToResources i res = players . ix (fromPlayerIndex i) . resources <>= res

doPurchase :: (RandomGen g) => PlayerIndex -> Item -> GameState g
doPurchase playerIndex' item' = do
  case item' of
    (Building r@(Roadway (OnEdge _ _))) ->
      replaceInventory playerIndex' (deconstruct r) (Just item') $ return ()
    (Building s@(Edifice (OnPoint _ _ Settlement))) ->
      replaceInventory playerIndex' (deconstruct s) (Just item') $ return ()
    (Building c@(Edifice (OnPoint p' c' City))) ->
      replaceInventory playerIndex' (deconstruct c) (Just item') $ do
        let settlementWas = Building (Edifice (OnPoint p' c' Settlement))
        replaceInventory playerIndex' (settlementWas) (Just (unbuilt settlement)) $ return ()
    Card _ -> getCard playerIndex'
    Potential DevelopmentCard -> getCard playerIndex'
    Potential (H _) -> return () -- can't buy potential settlement without location
    Potential Road -> return () -- can't buy potential road without location

getCard :: (RandomGen g) => PlayerIndex -> GameState g
getCard playerIndex' = do
  allCards' <- use allCards
  let (cardTop, restOfCards) = splitAt 1 allCards'
  players . (ix $ fromPlayerIndex playerIndex') . constructed <>= cardTop
  allCards .= restOfCards

doTrade :: (RandomGen g) => PlayerIndex -> TradeAction -> GameState g
doTrade playerIndex' tradeAction' = do
  case tradeAction' of
    Offer _ -> openTrades <>= [tradeAction']
    Accept _ _ -> openTrades <>= [tradeAction']
    Reject _ _ _ -> openTrades <>= [tradeAction']
    CompleteTrade offer' accepterIndex' -> do
      players . (ix $ fromPlayerIndex playerIndex') . resources <>= offer'^.asking
      players . (ix $ fromPlayerIndex playerIndex') . resources <>= mkNeg (offer'^.offering)
      players . (ix $ fromPlayerIndex accepterIndex') . resources <>= offer'^.offering
      players . (ix $ fromPlayerIndex accepterIndex') . resources <>= mkNeg (offer'^.asking)
      openTrades .= []
    CancelTrade _ -> openTrades .= []
    Exchange offer' -> doExchange playerIndex' offer'

doExchange :: (RandomGen g) => PlayerIndex -> TradeOffer -> GameState g
doExchange playerIndex' offer' = do
  let askTotal = totalResources $ offer'^.asking
  harborFuncs <- getHarbors playerIndex'
  let canGetTotal = maximum $ map ($ offer'^.offering) $ (genericHarborDiscount 4):harborFuncs
  if askTotal <= canGetTotal
  then do
    addToResources playerIndex' $ offer'^.asking
    addToResources playerIndex' (mkNeg $ offer'^.offering)
  else return ()

getPlayerBuildings :: (RandomGen g) => PlayerIndex -> GameStateReturning g (Map.Map Point OnPoint)
getPlayerBuildings playerIndex' = do
  b <- use board
  player' <- getPlayer playerIndex'
  let color' = color player'
  return $ Map.mapMaybe id $ Map.filter (\x -> color `fmap` x == Just color') $ b^.buildings

getHarbors :: (RandomGen g) => PlayerIndex -> GameStateReturning g [ResourceCount -> Int]
getHarbors playerIndex' = do
  b <- use board
  playerBuildings <- getPlayerBuildings playerIndex'
  let harbors' = b^.harbors
  let myHarbors = Map.elems $ Map.intersectionWith (\_ harbor' -> harbor') playerBuildings harbors'
  return $ map (harborDiscount) myHarbors

doDiscard :: (RandomGen g) => PlayerIndex -> DiscardAction -> GameState g
doDiscard playerIndex' (DiscardAction _ r) = do
  players . ix (fromPlayerIndex playerIndex') . resources <>= (mkNeg r)

doPlayCard :: (RandomGen g) => PlayerIndex -> DevelopmentCard -> GameState g
doPlayCard playerIndex' card' = do
  replaceInventory playerIndex' (Card card') Nothing $ do
    case card' of
      RoadBuilding -> toSpecialPhase (FreeRoads 2) playerIndex'
      Knight -> toSpecialPhase MovingRobber playerIndex'
      Invention -> toSpecialPhase Inventing playerIndex'
      Monopoly -> toSpecialPhase Monopolizing playerIndex'
      VictoryPoint -> return ()

doMonopoly :: (RandomGen g) => PlayerIndex -> Monopoly -> GameState g
doMonopoly = assert False undefined

doInvention :: (RandomGen g) => PlayerIndex -> Invention -> GameState g
doInvention = assert False undefined

doMoveRobber :: (RandomGen g) => PlayerIndex -> MoveRobber -> GameState g
doMoveRobber = assert False undefined

doRoll :: (RandomGen g) => GameState g
doRoll = do
  d1 <- getRandomR (1, 6)
  d2 <- getRandomR (1, 6)
  let r' = d1 + d2
  rolled .= (Just r')
  updateForRoll

genPlayerActions :: (RandomGen g) => GameState g
genPlayerActions = do
  currentPlayerIndex <- use currentPlayer
  purchases' <- possiblePurchases currentPlayerIndex
  trades <- possibleTradeActions currentPlayerIndex
  cardsToPlay <- possibleDevelopmentCards currentPlayerIndex
  validActions .= purchases' ++ trades ++ cardsToPlay ++ [mkEndTurn currentPlayerIndex]

possiblePurchases :: (RandomGen g) => PlayerIndex -> GameStateReturning g [GameAction]
possiblePurchases playerIndex' = do
  player' <- getPlayer playerIndex'
  b <- use board
  let roads' = validRoadsFor (color player') b
  let settlements' = validSettlementsFor (color player') b
  let cities' = validCitiesFor (color player') b
  let cards' = if sufficient (player'^.resources) (cost $ Potential DevelopmentCard)
               then [purchase playerIndex' (Potential DevelopmentCard)]
               else []
  let mkPurchases = map ((purchase playerIndex') . Building)
  return $ cards' ++ (concatMap mkPurchases [roads', settlements', cities'])

possibleTradeActions :: (RandomGen g) => PlayerIndex -> GameStateReturning g [GameAction]
possibleTradeActions playerIndex' = do
  -- openTrades' <- use openTrades
  -- let accept' = accept 
  -- let completes = 
  return [mkOffer playerIndex' mempty mempty] -- :completes

possibleDevelopmentCards :: (RandomGen g) => PlayerIndex -> GameStateReturning g [GameAction]
possibleDevelopmentCards playerIndex' = do
  items' <- inventory playerIndex'
  let cards' = catMaybes $ map (\x -> x ^? card) items'
  return $ map (mkPlayCard playerIndex') cards'

canMoveRobberTo :: (RandomGen g) => PlayerIndex -> GameStateReturning g [GameAction]
canMoveRobberTo _ = return $ [] -- assert False undefined

-- | Called on EndTurn action or in initial phase
progress :: (RandomGen g) => GameState g
progress = do
  currentPlayerIndex' <- use currentPlayer
  let currentPlayerIndex = fromPlayerIndex currentPlayerIndex'
  adv <- use turnAdvanceBy
  totalPlayers <- uses players length
  p <- use phase
  let next' = toPlayerIndex $ (totalPlayers + currentPlayerIndex + adv) `mod` totalPlayers
  let newAdv x | x == 3 && adv == 1 = 0
               | x == 3 && adv == 0 = -1
               | otherwise = adv
  nextPlayer <- uses players ((flip (!!)) $ fromPlayerIndex next')
  nextActionsIfInitial <- uses board (possibleInitialSettlements nextPlayer)
  case p of
    Initial -> if (currentPlayerIndex == 0 && adv == -1) 
               then do
                turnAdvanceBy .= 1
                initialToNormal
               else do
                 currentPlayer .= next'
                 validActions .= nextActionsIfInitial
                 turnAdvanceBy .= newAdv (fromPlayerIndex next')
    Normal -> switchToPlayer next' [rollFor next']
    Special _ -> return () -- if progress called in a special phase, do nothing
    End -> return ()

switchToPlayer :: (RandomGen g) => PlayerIndex -> [GameAction] -> GameState g
switchToPlayer i valids = do
  currentPlayer .= i
  validActions .= valids

-- * State transitions

updateForRoll :: (RandomGen g) => GameState g
updateForRoll = do
  r' <- use rolled
  currentPlayer' <- use currentPlayer
  case r' of
    Just 7 -> toSpecialPhase RobberAttack currentPlayer'
    _ -> do
      distributeResources
      genPlayerActions

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
  validActions .= [rollFor (toPlayerIndex 0)]

backToNormal :: (RandomGen g) => GameState g
backToNormal = do
  phase .= Normal
  genPlayerActions

toSpecialPhase :: (RandomGen g) => SpecialPhase -> PlayerIndex -> GameState g
toSpecialPhase special' playerIndex' = do
  phase .= Special special'
  case special' of
      FreeRoads _ -> do
        player' <- getPlayer playerIndex'
        possibleRoads <- uses board (validRoadsFor $ color player')
        validActions .= map (mkFree playerIndex') possibleRoads
      RobberAttack -> do -- ^ If no one has more than 7 resources, move on to MovingRobber.
        ps <- use players
        let pRes = map (\p -> (p^.playerIndex, totalResources (p ^. resources))) ps
        let mustDiscard = map mkDiscard $ filter (\(_, r) -> r > 7) pRes
        if (length mustDiscard > 0)
        then do
          validActions .= mustDiscard
        else toSpecialPhase MovingRobber playerIndex'
      MovingRobber -> do
        robberMoves <- canMoveRobberTo playerIndex'
        if null robberMoves
        then backToNormal
        else validActions .= robberMoves
      Inventing -> do
        validActions .= map (invent playerIndex') possibleInventions
      Monopolizing -> do
        validActions .= map (PlayerAction playerIndex' . SpecialAction) possibleMonopolies

-- | Game won by player p.
wonBy :: (RandomGen g) => PlayerIndex -> GameState g
wonBy pI = do
  phase .= End
  winner .= Just pI

-- | Choose a valid action at random (useful for testing purposes)
randomAct :: (RandomGen g) => GameState g
randomAct = do
  vA <- use validActions
  act' <- uniform vA
  update act'