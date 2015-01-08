{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Cataskell.Game where

import Control.Monad.Identity
import Control.Monad.Random
import Control.Monad.State
import Control.Monad ((>=>))
import Control.Exception (assert)
import System.Random.Shuffle
import qualified Data.Map.Strict as Map
import Data.Monoid (mempty, (<>))
import Data.Maybe (fromJust, isJust, mapMaybe, listToMaybe)
import Data.List (find, findIndex, elemIndex, nub)
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
  , _lastAction :: Maybe (GameAction, Bool)
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
  return Game { _phase = Initial
              , _board = b
              , _players = ps
              , _currentPlayer = toPlayerIndex 0
              , _turnAdvanceBy = 1
              , _rolled = Nothing
              , _validActions = possibleInitialSettlements (head ps) b
              , _openTrades = []
              , _lastAction = Nothing
              , _allCards = cards
              , _winner = Nothing }

runGame :: (RandomGen g) => GameState g -> Game -> g -> (Game, g)
runGame stModify game = runRand (execStateT stModify game)

evalGame :: (RandomGen g) => GameStateReturning g a -> Game -> g -> a
evalGame stReturn game = evalRand (evalStateT stReturn game)

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

  lastAction .= Just (action', done)
  when (done && builtFreeRoad) $ if p == Initial then progress else handleFreeRoads p
  when (done && p == Special MovingRobber && movedRobber) backToNormal

-- | Move from one game state to the next
handleFreeRoads :: (RandomGen g) => Phase -> GameState g
handleFreeRoads phase' = do
  playerIndex' <- use currentPlayer
  when (phase' == Special (FreeRoads 2)) $ toSpecialPhase (FreeRoads 1) playerIndex'
  when (phase' == Special (FreeRoads 1)) backToNormal
  return ()

-- | Checks that the action is valid and executes it if so
checkAndExecute :: (RandomGen g) => GameAction -> GameStateReturning g Bool
checkAndExecute action' = do
  valid <- isValid action'
  when valid $ doAction action'
  return valid

-- | Returns true iff all preconditions are met
isValid :: (RandomGen g) => GameAction -> GameStateReturning g Bool
isValid action' = do
  current <- get
  let predicates' = preconditions action'
  let valid = all ($ current) predicates'
  return valid

-- | Creates a predicate that is true when a game is in one of the specified phases.
phaseOneOf :: [Phase] -> Game -> Bool
phaseOneOf phases = flip elem phases . view phase

-- | Creates a predicate that checks if a player has an item
hasItem :: Item -> PlayerIndex -> Game -> Bool
hasItem itemToFind pI = elem itemToFind . playerItems
  where playerItems = view (players.ix (fromPlayerIndex pI).constructed)

-- | Creates a predicate that checks if a player has enough resources for something.
hasResourcesFor :: ResourceCount -> PlayerIndex -> Game -> Bool
hasResourcesFor cost' pI = flip sufficient cost' . playerResources
  where playerResources = view (players.ix (fromPlayerIndex pI).resources)

-- | Creates a predicate that checks if a player has enough resources for an item.
hasResourcesForItem :: Item -> PlayerIndex -> Game -> Bool
hasResourcesForItem itemToBuy = hasResourcesFor (cost itemToBuy)

getPlayer :: (RandomGen g) => PlayerIndex -> GameStateReturning g Player
getPlayer pI = do
  current <- get
  return $ current ^?! players . ix (fromPlayerIndex pI)

scoreFor :: (RandomGen g) => PlayerIndex -> GameStateReturning g Int
scoreFor pI = do
  player' <- getPlayer pI
  return $ view score player'

scores :: (RandomGen g) => GameStateReturning g [Int]
scores = do
  ps <- use players
  return $ map (view score) ps

turnIs :: PlayerIndex -> Game -> Bool
turnIs playerIndex' = (== playerIndex') . view currentPlayer

-- | Returns a series of predicates that must be true for an action to proceed.
preconditions :: GameAction -> [Game -> Bool]
preconditions (PlayerAction playerIndex' action')
  = case action' of
      Roll -> [ phaseOneOf [Normal]
              , turnIs playerIndex'
              , \g -> view currentPlayer g == playerIndex']
      BuildForFree _ -> [phaseOneOf [Initial, Special (FreeRoads 2), Special (FreeRoads 1)], turnIs playerIndex']
      SpecialAction x -> case x of
        M _ -> [phaseOneOf [Special Monopolizing], turnIs playerIndex']
        I _ -> [phaseOneOf [Special Inventing], turnIs playerIndex']
        R _ -> [phaseOneOf [Special MovingRobber], turnIs playerIndex']
      PlayCard x -> [ phaseOneOf [Normal]
                    , hasItem (Card x) playerIndex'
                    , turnIs playerIndex'
                    , \_ -> x /= VictoryPoint] -- can't play a victory point
      Purchase x -> [ phaseOneOf [Normal]
                    , turnIs playerIndex'
                    , hasResourcesForItem x playerIndex']
      Trade x -> case x of
        Offer offer' -> [phaseOneOf [Normal], hasResourcesFor (offer'^.offering) playerIndex', turnIs playerIndex']
        Accept offer' _ -> [phaseOneOf [Normal], hasResourcesFor (offer'^.asking) playerIndex']
        Reject {} -> [phaseOneOf [Normal]]
        CompleteTrade offer' accepterIndex' -> [ phaseOneOf [Normal]
                                               , hasResourcesFor (offer'^.offering) playerIndex'
                                               , hasResourcesFor (offer'^.asking) accepterIndex'
                                               , turnIs playerIndex']
        CancelTrade _ -> [phaseOneOf [Normal], turnIs playerIndex']
        Exchange offer' -> [phaseOneOf [Normal], hasResourcesFor (offer'^.offering) playerIndex', turnIs playerIndex']
      Discard (DiscardAction i r) -> [ phaseOneOf [Special RobberAttack]
                                     , \_ -> totalResources r == i ]
      EndTurn -> [phaseOneOf [Normal], turnIs playerIndex']

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
            when (deconstruct c' == Potential (H Settlement)) $ do
              when (totalSettlements == 2) $ giveStartingResources actorIndex
              let c'' = fromJust $ c' ^? onPoint
              newRoads <- uses (board.roads) (initialRoadsFor player' c'')
              validActions .= newRoads
          SpecialAction x -> case x of
            M monopoly' -> doMonopoly actorIndex monopoly'
            I invention' -> doInvention actorIndex invention'
            R moveRobber' -> doMoveRobber moveRobber'
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
  replaceInventory p' potential (Just $ Building construct') $ board .= build construct' b

inventory :: (RandomGen g) => PlayerIndex -> GameStateReturning g [Item]
inventory playerIndex' = liftM (view constructed) $ getPlayer playerIndex'

replaceInventory :: (RandomGen g) => PlayerIndex -> Item -> Maybe Item -> GameState g -> GameState g
replaceInventory playerIndex' old' new' extraActions = do
  hasLeft <- use (players . ix (fromPlayerIndex playerIndex') . constructed)
  let i = elemIndex old' hasLeft
  when (isJust i) $ do
    let i' = fromJust i
    if isJust new'
    then players . ix (fromPlayerIndex playerIndex') . constructed . ix i' .= fromJust new'
    else players . ix (fromPlayerIndex playerIndex') . constructed .= hasLeft ^.. folded . ifiltered (\i'' _ -> i'' /= i')
    extraActions

addToResources :: (RandomGen g) => PlayerIndex -> ResourceCount -> GameState g
addToResources i res = players . ix (fromPlayerIndex i) . resources <>= res

doPurchase :: (RandomGen g) => PlayerIndex -> Item -> GameState g
doPurchase playerIndex' item'
  = case item' of
      (Building r@(Roadway (OnEdge _ _))) ->
        replaceInventory playerIndex' (deconstruct r) (Just item') $ return ()
      (Building s@(Edifice (OnPoint _ _ Settlement))) ->
        replaceInventory playerIndex' (deconstruct s) (Just item') $ return ()
      (Building c@(Edifice (OnPoint p' c' City))) ->
        replaceInventory playerIndex' (deconstruct c) (Just item') $ do
          let settlementWas = Building (Edifice (OnPoint p' c' Settlement))
          replaceInventory playerIndex' settlementWas (Just (unbuilt settlement)) $ return ()
      Card _ -> getCard playerIndex'
      Potential DevelopmentCard -> getCard playerIndex'
      Potential (H _) -> return () -- can't buy potential settlement without location
      Potential Road -> return () -- can't buy potential road without location

getCard :: (RandomGen g) => PlayerIndex -> GameState g
getCard playerIndex' = do
  allCards' <- use allCards
  let (cardTop, restOfCards) = splitAt 1 allCards'
  players . ix (fromPlayerIndex playerIndex') . constructed <>= cardTop
  allCards .= restOfCards

doTrade :: (RandomGen g) => PlayerIndex -> TradeAction -> GameState g
doTrade playerIndex' tradeAction'
  = case tradeAction' of
      Offer _ -> addAndUpdateTrades tradeAction'
      Accept _ _ -> addAndUpdateTrades tradeAction'
      Reject{} -> addAndUpdateTrades tradeAction'
      CompleteTrade offer' accepterIndex' -> do
        players . ix (fromPlayerIndex playerIndex') . resources <>= offer'^.asking
        players . ix (fromPlayerIndex playerIndex') . resources <>= mkNeg (offer'^.offering)
        players . ix (fromPlayerIndex accepterIndex') . resources <>= offer'^.offering
        players . ix (fromPlayerIndex accepterIndex') . resources <>= mkNeg (offer'^.asking)
        openTrades .= []
      CancelTrade _ -> openTrades .= []
      Exchange offer' -> doExchange playerIndex' offer'

addAndUpdateTrades :: (RandomGen g) => TradeAction -> GameState g
addAndUpdateTrades tradeAction' = do
  openTrades <>= [tradeAction']
  newTrades <- possibleTradeActions
  validActions' <- use validActions
  validActions .= nub (validActions' ++ newTrades)

doExchange :: (RandomGen g) => PlayerIndex -> TradeOffer -> GameState g
doExchange playerIndex' offer' = do
  let askTotal = totalResources $ offer'^.asking
  harborFuncs <- getHarbors playerIndex'
  let canGetTotal = maximum $ map ($ offer'^.offering) $ genericHarborDiscount 4:harborFuncs
  when (askTotal <= canGetTotal) $ do
    addToResources playerIndex' $ offer'^.asking
    addToResources playerIndex' (mkNeg $ offer'^.offering)

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
  return $ map harborDiscount myHarbors

doDiscard :: (RandomGen g) => PlayerIndex -> DiscardAction -> GameState g
doDiscard playerIndex' (DiscardAction _ r)
  = players . ix (fromPlayerIndex playerIndex') . resources <>= mkNeg r

doPlayCard :: (RandomGen g) => PlayerIndex -> DevelopmentCard -> GameState g
doPlayCard playerIndex' card'
  = replaceInventory playerIndex' (Card card') Nothing $ case card' of
      RoadBuilding -> toSpecialPhase (FreeRoads 2) playerIndex'
      Knight -> toSpecialPhase MovingRobber playerIndex'
      Invention -> toSpecialPhase Inventing playerIndex'
      Monopoly -> toSpecialPhase Monopolizing playerIndex'
      VictoryPoint -> return ()

myFoldM :: Monad m => a -> [b] -> (a -> b -> m a) -> m a
myFoldM a1 lst f = foldM f a1 lst

doMonopoly :: (RandomGen g) => PlayerIndex -> Monopoly -> GameState g
doMonopoly playerIndex' (MonopolyOn resType) = do
  ptotal <- uses players length
  newSum <- myFoldM mempty [0 .. ptotal] $ \a b ->
    if b /= fromPlayerIndex playerIndex'
    then do
      res <- use (players . ix b . resources)
      let res' = filteredResCount resType res
      players . ix b . resources <>= mkNeg res'
      return $ a <> res'
    else return a
  players . ix (fromPlayerIndex playerIndex') . resources <>= newSum
  backToNormal

doInvention :: (RandomGen g) => PlayerIndex -> Invention -> GameState g
doInvention playerIndex' (InventionOf res)
  = players . ix (fromPlayerIndex playerIndex') . resources <>= res

doMoveRobber :: (RandomGen g) => MoveRobber -> GameState g
doMoveRobber (MoveRobber dest) = do
  h <- use (board . hexes)
  let (robberLoc, _) = fromJust $ find (_hasRobber . snd) $ Map.toList h
  board . hexes . ix robberLoc . hasRobber .= False
  board . hexes . ix dest . hasRobber .= True
  backToNormal

doRoll :: (RandomGen g) => GameState g
doRoll = do
  d1 <- getRandomR (1, 6)
  d2 <- getRandomR (1, 6)
  let r' = d1 + d2
  rolled .= Just r'
  updateForRoll

genPlayerActions :: (RandomGen g) => GameState g
genPlayerActions = do
  currentPlayerIndex <- use currentPlayer
  purchases' <- possiblePurchases currentPlayerIndex
  trades <- possibleTradeActions
  cardsToPlay <- possibleDevelopmentCards currentPlayerIndex
  validActions .= purchases' ++ trades ++ cardsToPlay ++ [mkEndTurn currentPlayerIndex]

possiblePurchases :: (RandomGen g) => PlayerIndex -> GameStateReturning g [GameAction]
possiblePurchases playerIndex' = do
  player' <- getPlayer playerIndex'
  b <- use board
  let roads' = validRoadsFor (color player') b
  let settlements' = validSettlementsFor (color player') b
  let cities' = validCitiesFor (color player') b
  let buildings' = concat [roads', settlements', cities']
  let res = player'^.resources
  let possibleBuildings = filter (\x -> sufficient res (cost $ deconstruct x)) buildings'
  let cards' = if sufficient res (cost $ Potential DevelopmentCard)
               then [purchase playerIndex' (Potential DevelopmentCard)]
               else []
  let mkPurchase = purchase playerIndex' . Building
  return $ cards' ++ map mkPurchase possibleBuildings

possibleTradeActions :: (RandomGen g) => GameStateReturning g [GameAction]
possibleTradeActions = do
  openTrades' <- use openTrades
  playerIndex' <- use currentPlayer
  let base = [mkOffer playerIndex' mempty mempty]
  let offer' = listToMaybe $ mapMaybe toOffer openTrades'
  if isJust offer'
  then do
    let offer'' = fromJust offer'
    acceptances <- mkAccept offer''
    others <- otherPlayers
    let otherIs = map (view playerIndex) others
    let rejects = map (reject offer'' Nothing) otherIs
    let existingAccepts = filter isAccept openTrades'
    let acceptances' = filter (\a -> not $ any ((== Just (a^.actor)) . preview accepter) existingAccepts) acceptances
    let completes = mapMaybe complete existingAccepts
    let cancels = if (offer''^.offeredBy == playerIndex') then [cancel offer''] else []
    assert (null existingAccepts || not (null completes)) return $ acceptances' ++ rejects ++ cancels ++ completes
  else return base

otherPlayers :: (RandomGen g) => GameStateReturning g [Player]
otherPlayers = do
  current <- use currentPlayer
  uses players (filter ((/= current) . view playerIndex))

mkAccept :: (RandomGen g) => TradeOffer -> GameStateReturning g [GameAction]
mkAccept offer' = do
  ps <- otherPlayers
  let ps' = filter (\p -> sufficient (view resources p) (offer'^.asking)) ps
  let couldAccept = map (view playerIndex) ps'
  return $ map (accept offer') couldAccept

possibleDevelopmentCards :: (RandomGen g) => PlayerIndex -> GameStateReturning g [GameAction]
possibleDevelopmentCards playerIndex' = do
  items' <- inventory playerIndex'
  let cards' = filter (/= VictoryPoint) $ mapMaybe (^? card) items'
  return $ map (mkPlayCard playerIndex') cards'

canMoveRobberTo :: (RandomGen g) => PlayerIndex -> GameStateReturning g [GameAction]
canMoveRobberTo playerIndex' = do
  robberSpots <- robbableSpotsFor playerIndex'
  return $ map (mkMoveRobber playerIndex') robberSpots

neighborBuildings :: (RandomGen g) => CentralPoint -> GameStateReturning g [OnPoint]
neighborBuildings cp = do
  b <- use board
  let buildings' = getHabitations b
  let myNeighbors = (Map.!) centersToNeighbors cp
  return $ mapMaybe (`Map.lookup` buildings') myNeighbors

robbableSpotsFor :: (RandomGen g) => PlayerIndex -> GameStateReturning g [CentralPoint]
robbableSpotsFor playerIndex' = do
  ps <- use players
  self <- getPlayer playerIndex'
  let ownColor = color self
  let playersColors = zip ps (map color ps)
  let protected = nub $ ownColor:(map snd $ filter (\(p,_) -> view displayScore p < 2) playersColors)
  filterM (neighborBuildings >=> return . all (not . flip elem protected) . map color) hexCenterPoints

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
  nextPlayer <- uses players (flip (!!) $ fromPlayerIndex next')
  nextActionsIfInitial <- uses board (possibleInitialSettlements nextPlayer)
  case p of
    Initial -> if currentPlayerIndex == 0 && adv == -1
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
        mustDiscard <- makeDiscards
        if not $ null mustDiscard
        then validActions .= mustDiscard
        else toSpecialPhase MovingRobber playerIndex'
      MovingRobber -> do
        robberMoves <- canMoveRobberTo playerIndex'
        if null robberMoves
        then backToNormal
        else validActions .= robberMoves
      Inventing -> validActions .= map (invent playerIndex') possibleInventions
      Monopolizing -> validActions .= map (PlayerAction playerIndex' . SpecialAction) possibleMonopolies

makeDiscards :: (RandomGen g) => GameStateReturning g [GameAction]
makeDiscards = do
  ps <- use players
  let pRes = map (\p -> (p^.playerIndex, p ^. resources)) ps
  let pMustDiscard = filter (\(_, r) -> totalResources r > 7) pRes
  let listDiscards (p,r) = let total' = totalResources r `div` 2
                               rs = resCombinationsForTotal total'
                           in  zip (repeat p) $ filter (sufficient r) rs
  let pDiscards = concatMap listDiscards pMustDiscard
  return $ map mkDiscard pDiscards

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

-- | Force a particular roll and update (useful for testing purposes).
forceRoll :: (RandomGen g) => Int -> GameState g
forceRoll i = do
  rolled .= Just i
  updateForRoll
