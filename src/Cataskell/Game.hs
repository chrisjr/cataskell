{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Cataskell.Game where

import Control.Monad.Identity
import Control.Monad.Random
import Control.Monad.State
import Control.Exception (assert)
import System.Random.Shuffle
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Applicative ((<$>), (<*>), (<|>))
import Data.Either
import Data.Monoid (mempty, (<>))
import Data.Maybe
import Data.List (elemIndex, nub)
import Cataskell.GameData.Actions
import Cataskell.GameData.Basics
import Cataskell.GameData.Board
import Cataskell.GameData.Location
import Cataskell.GameData.Player
import Cataskell.GameData.Preconditions
import Cataskell.GameData.Resources
import Cataskell.Util
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
  , _players :: Map.Map PlayerIndex Player
  , _currentPlayer :: PlayerIndex
  , _turnAdvanceBy :: Int
  , _rolled :: Maybe Int
  , _validActions :: Set GameAction
  , _openTrades :: Set TradeAction
  , _lastAction :: Maybe (GameAction, Bool)
  , _allCards :: [DevelopmentCard]
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
              , _validActions = possibleInitialSettlements (head $ Map.elems ps) b
              , _openTrades = Set.empty
              , _lastAction = Nothing
              , _allCards = cards
              , _winner = Nothing }

runGame :: (RandomGen g) => GameState g -> Game -> g -> (Game, g)
runGame stModify game = runRand (execStateT stModify game)

evalGame :: (RandomGen g) => GameStateReturning g a -> Game -> g -> a
evalGame stReturn game = evalRand (evalStateT stReturn game)

findPlayerByColor :: (RandomGen g) => Color -> GameStateReturning g PlayerIndex
findPlayerByColor c = do
  maybeI <- uses players $ findKeyWhere ((== c) . color)
  return $ fromJust maybeI

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

-- | Move from one game state to the next when a RoadBuilding card was played
handleFreeRoads :: (RandomGen g) => Phase -> GameState g
handleFreeRoads phase' = do
  playerIndex' <- use currentPlayer
  updateBonuses
  when (phase' == Special (FreeRoads 2)) $ toSpecialPhase (FreeRoads 1) playerIndex'
  when (phase' == Special (FreeRoads 1)) backToNormal

-- | Checks that the action is valid and executes it if so
checkAndExecute :: (RandomGen g) => GameAction -> GameStateReturning g Bool
checkAndExecute action' = do
  validAct <- isValid action'
  when validAct $ doAction action'
  return validAct

-- | Returns true iff all preconditions are met
isValid :: (RandomGen g) => GameAction -> GameStateReturning g Bool
isValid action' = do
  current <- get
  let predicates' = preconditions action'
  let validAct = isRight $ checkAll predicates' current
  -- let validAct = checkAllUnsafe predicates' current -- will throw errors on precondition failure, for debugging
  return validAct

-- | Creates a predicate that is true when a game is in one of the specified phases.
phaseOneOf :: [Phase] -> Precondition Game
phaseOneOf phases = Precondition { predicate = flip elem phases . view phase, label = "phase one of: " ++ show phases } 

playersExist'' :: Set PlayerIndex -> Game -> Bool
playersExist'' pIs g = let keys = Map.keysSet (g^.players)
                       in pIs `Set.isSubsetOf` keys

playersExist' :: [PlayerIndex] -> Game -> Bool
playersExist' pIs = playersExist'' (Set.fromList pIs)

playersReferencedInTrade :: TradeAction -> Set PlayerIndex
playersReferencedInTrade x
  = Set.fromList $ case x of
                     Offer offer' -> [offer'^.offeredBy]
                     Accept offer' accepter' -> [offer'^.offeredBy, accepter']
                     Reject offer' rejecter' _ -> [offer'^.offeredBy, rejecter']
                     CompleteTrade offer' accepter' -> [offer'^.offeredBy, accepter']
                     CancelTrade offer' -> [offer'^.offeredBy]
                     Exchange offer' -> [offer'^.offeredBy]

playersReferenced :: GameAction -> Set PlayerIndex
playersReferenced (PlayerAction actor' act') = Set.union (Set.singleton actor') fromAct
  where fromAct = case act' of
                    Trade x -> playersReferencedInTrade x
                    _ -> Set.empty

playersExistFor' :: GameAction -> Game -> Bool
playersExistFor' = playersExist'' . playersReferenced

playersExistFor :: GameAction -> Precondition Game
playersExistFor a = Precondition { predicate = playersExistFor' a, label = "All players referenced by " ++ show a }

-- | Creates a predicate that checks if a player has an item
hasItem :: Item -> PlayerIndex -> Precondition Game
hasItem itemToFind pI = Precondition { predicate = hasIt, label = show pI ++ " has a " ++ show itemToFind }
  where hasIt = elem itemToFind . playerItems
        playerItems = view (players.ix pI.constructed)

-- | Creates a predicate that checks if a player has enough resources for something.
hasResourcesFor :: ResourceCount -> PlayerIndex -> Precondition Game
hasResourcesFor cost' pI = Precondition { predicate = enough, label = show pI ++ " has enough for " ++ show cost' }
  where enough = flip sufficient cost' . playerResources
        playerResources = view (players.ix pI.resources)

-- | Creates a predicate that checks if a player has enough resources for an item.
hasResourcesForItem :: Item -> PlayerIndex -> Precondition Game
hasResourcesForItem itemToBuy = hasResourcesFor (cost itemToBuy)

checkM :: Precondition Game -> GameStateReturning g Bool
checkM p = do
  let pred' = predicate p
  fmap pred' get

checkResourcesForCompleteTrade :: TradeOffer -> PlayerIndex -> GameStateReturning g Bool
checkResourcesForCompleteTrade offer' accepter' = do
  let offerer' = offer'^.offeredBy
  offeringValid <- checkM $ hasResourcesFor (offer'^.offering) offerer'
  askingValid <- checkM $ hasResourcesFor (offer'^.asking) accepter'
  return $ offeringValid && askingValid

-- | Check that an offer originated with the ourrent player
offerOriginatedWith :: TradeOffer -> PlayerIndex -> Precondition Game
offerOriginatedWith offer' playerIndex'
  = Precondition (\_ -> offer'^.offeredBy == playerIndex') ("offer originated by " ++ show playerIndex')

-- | Check that a response didn't come from the person who made the offer (unless a complete or a cancel)
responseNotFromSamePlayer :: TradeAction -> Precondition Game
responseNotFromSamePlayer trade'
  = Precondition (\_ -> Just (trade'^.offer.offeredBy) /= otherParty) "Trade response is from a different player"
      where otherParty = (trade' ^? accepter) <|> (trade' ^? rejecter)

scoreFor :: (RandomGen g) => PlayerIndex -> GameStateReturning g (Maybe Int)
scoreFor pI = preuse (players.ix pI.score)

scores :: (RandomGen g) => GameStateReturning g [Int]
scores = do
  ps <- use players
  return . Map.elems $ Map.map (view score) ps

turnIs :: PlayerIndex -> Precondition Game
turnIs playerIndex' = Precondition {predicate = (== playerIndex') . view currentPlayer, label = "it's " ++ show playerIndex' ++ "'s turn"}

-- | Returns a series of predicates that must be true for an action to proceed.
preconditions :: GameAction -> [Precondition Game]
preconditions a@(PlayerAction playerIndex' action') = playersExistFor a : reqs
  where reqs = case action' of
                 Roll -> [ phaseOneOf [Normal]
                         , turnIs playerIndex'
                         ]
                 BuildForFree _ -> [phaseOneOf [Initial, Special (FreeRoads 2), Special (FreeRoads 1)], turnIs playerIndex']
                 SpecialAction x -> case x of
                   M _ -> [phaseOneOf [Special Monopolizing], turnIs playerIndex']
                   I _ -> [phaseOneOf [Special Inventing], turnIs playerIndex']
                   R _ -> [phaseOneOf [Special MovingRobber], turnIs playerIndex']
                 PlayCard x -> [ phaseOneOf [Normal]
                               , hasItem (Card x) playerIndex'
                               , turnIs playerIndex'
                               , Precondition( \_ -> x /= VictoryPoint) "VP is unplayable"]
                 Purchase x -> [ phaseOneOf [Normal]
                               , turnIs playerIndex'
                               , hasResourcesForItem x playerIndex'
                               , Precondition (\g -> let construct' = x ^? building
                                                         maybeValid = do
                                                           c <- construct'
                                                           return $ validConstruct c (g ^. board)
                                                     in  maybeValid == Just True || isNothing maybeValid) (show x ++ " is valid on board") ]
                 Trade x -> responseNotFromSamePlayer x : case x of
                   Offer offer' -> [ phaseOneOf [Normal]
                                   , hasResourcesFor (offer'^.offering) playerIndex'
                                   , turnIs playerIndex'
                                   , Precondition (\_ -> (offer'^.offering) /= (offer'^.asking)) "Offer and ask cannot be the same"]
                   Accept offer' accepter' -> [phaseOneOf [Normal], hasResourcesFor (offer'^.asking) accepter']
                   Reject {} -> [phaseOneOf [Normal]]
                   CompleteTrade offer' accepterIndex' -> [ phaseOneOf [Normal]
                                                          , offerOriginatedWith offer' playerIndex'
                                                          , hasResourcesFor (offer'^.offering) playerIndex'
                                                          , hasResourcesFor (offer'^.asking) accepterIndex'
                                                          , turnIs playerIndex']
                   CancelTrade offer' -> [phaseOneOf [Normal], offerOriginatedWith offer' playerIndex', turnIs playerIndex']
                   Exchange offer' -> [phaseOneOf [Normal], hasResourcesFor (offer'^.offering) playerIndex', turnIs playerIndex']
                 Discard (DiscardAction i r) -> [ phaseOneOf [Special RobberAttack]
                                                , Precondition (\_ -> totalResources r == i) (show action' ++ "satisfies minimum " ++ show i)]
                 EndTurn -> [phaseOneOf [Normal], turnIs playerIndex']

doAction :: (RandomGen g) => GameAction -> GameState g
doAction act'
  = let action' = act' ^. action
        actorIndex  = act' ^. actor
    in  case action' of
          Roll -> doRoll
          BuildForFree c' -> do
            doBuild actorIndex c'
            player' <- preuse (players . ix actorIndex)
            let player'' = assert (isJust player') fromJust player'
            let totalSettlements = length . filter isSettlement $ player'' ^. constructed
            when ((c'^?onPoint.buildingType) == Just Settlement) $ do
              when (totalSettlements == 2) $ giveStartingResources actorIndex
              let c'' = fromJust $ c' ^? onPoint
              newRoads <- uses (board.roads) (initialRoadsFor player'' c'')
              validActions .= Set.fromList newRoads
          SpecialAction x -> case x of
            M monopoly' -> doMonopoly actorIndex monopoly'
            I invention' -> doInvention actorIndex invention'
            R moveRobber' -> doMoveRobber moveRobber'
          Purchase x -> doPurchase actorIndex x >> updateBonuses >> genPlayerActions
          Trade x -> doTrade actorIndex x >> genPlayerActions
          Discard x -> doDiscard actorIndex x
          PlayCard x -> doPlayCard actorIndex x
          EndTurn -> do
            transferCards actorIndex
            scoreIsNow <- scoreFor actorIndex
            if scoreIsNow >= Just 10
            then wonBy actorIndex
            else progress

giveStartingResources :: (RandomGen g) => PlayerIndex -> GameState g
giveStartingResources pI = do
  hasBuilt <- inventory pI
  let lastSettlement = (last . filter isSettlement) `fmap` hasBuilt
  when (isJust lastSettlement) $ do
    let lastSettlement' = fromJust lastSettlement
    let p' = fromJust $ lastSettlement' ^? building.onPoint
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

inventory :: (RandomGen g) => PlayerIndex -> GameStateReturning g (Maybe [Item])
inventory playerIndex' = preuse (players.ix playerIndex'.constructed)

replaceInventory :: (RandomGen g) => PlayerIndex -> Item -> Maybe Item -> GameState g -> GameState g
replaceInventory playerIndex' old' new' extraActions = do
  hasLeft <- inventory playerIndex'
  let i = join $ elemIndex old' `fmap` hasLeft
  when (isJust i) $ do
    let hasLeft' = fromJust hasLeft
    let i' = fromJust i
    if isJust new'
    then players . ix playerIndex' . constructed . ix i' .= fromJust new'
    else players . ix playerIndex' . constructed .= hasLeft' ^.. folded . ifiltered (\i'' _ -> i'' /= i')
    extraActions

-- | Add specific amount to player's resources. Throws error on a negative result.
addToResources :: (RandomGen g) => PlayerIndex -> ResourceCount -> GameState g
addToResources pI res = do 
  resOld <- resourcesOf pI
  let res' = resOld <> res
  unless (nonNegative res') $ error ("tried to add " ++ show res ++ " to " ++ show resOld)
  players . ix pI . resources .= res'

resourcesOf :: (RandomGen g) => PlayerIndex -> GameStateReturning g ResourceCount
resourcesOf pI = use (players . ix pI . resources)

payFor :: (RandomGen g) => PlayerIndex -> Item -> GameState g
payFor playerIndex' item' = addToResources playerIndex' (mkNeg $ cost item')

doPurchase :: (RandomGen g) => PlayerIndex -> Item -> GameState g
doPurchase playerIndex' item' = do
  payFor playerIndex' item'
  case item' of
      (Building r@(Roadway (OnEdge _ _))) ->
        doBuild playerIndex' r
      (Building s@(Edifice (OnPoint _ _ Settlement))) ->
        doBuild playerIndex' s
      (Building c@(Edifice (OnPoint p' c' City))) -> do
        let settlementWas = Building . built $ settlement (Just (p', c'))
        replaceInventory playerIndex' settlementWas (Just $ unbuilt settlement) $ return ()
        doBuild playerIndex' c
      Card _ -> getCard
      Potential DevelopmentCard -> getCard
      Potential (H _) -> return () -- can't buy potential settlement without location
      Potential Road -> return () -- can't buy potential road without location

getCard :: (RandomGen g) => GameState g
getCard = do
  currentPlayer' <- use currentPlayer
  allCards' <- use allCards
  let (cardTop, restOfCards) = splitAt 1 allCards'
  players . ix currentPlayer' . newCards <>= cardTop
  allCards .= restOfCards

transferCards :: (RandomGen g) => PlayerIndex -> GameState g
transferCards pI = do
  cards' <- use (players . ix pI . newCards)
  players . ix pI . constructed <>= map Card cards'
  players . ix pI . newCards .= []

doTrade :: (RandomGen g) => PlayerIndex -> TradeAction -> GameState g
doTrade playerIndex' tradeAction'
  = case tradeAction' of
      Offer _ -> openTrades <>= Set.singleton tradeAction'
      Accept _ _ -> openTrades <>= Set.singleton tradeAction'
      Reject{} -> openTrades <>= Set.singleton tradeAction'
      CompleteTrade offer' accepterIndex' -> do
        addToResources playerIndex' (offer'^.asking)
        addToResources playerIndex' (mkNeg $ offer'^.offering)
        addToResources accepterIndex' (offer'^.offering)
        addToResources accepterIndex' (mkNeg $ offer'^.asking)
        openTrades .= Set.empty
      CancelTrade _ -> openTrades .= Set.empty
      Exchange offer' -> doExchange playerIndex' offer'

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
  color' <- preuses (players.ix playerIndex') color
  return $ Map.mapMaybe id $ Map.filter (\x -> isJust color' && color `fmap` x == color') $ b^.buildings

getHarbors :: (RandomGen g) => PlayerIndex -> GameStateReturning g [ResourceCount -> Int]
getHarbors playerIndex' = do
  b <- use board
  playerBuildings <- getPlayerBuildings playerIndex'
  let harbors' = b^.harbors
  let myHarbors = Map.elems $ Map.intersectionWith (\_ harbor' -> harbor') playerBuildings harbors'
  return $ map harborDiscount myHarbors

doDiscard :: (RandomGen g) => PlayerIndex -> DiscardAction -> GameState g
doDiscard playerIndex' (DiscardAction _ r) = do 
  addToResources playerIndex' (mkNeg r)
  discards <- makeDiscards
  validActions .= discards
  when (Set.null discards) $ do
    pI <- use currentPlayer
    toSpecialPhase MovingRobber pI

doPlayCard :: (RandomGen g) => PlayerIndex -> DevelopmentCard -> GameState g
doPlayCard playerIndex' card'
  = replaceInventory playerIndex' (Card card') Nothing $ case card' of
      RoadBuilding -> toSpecialPhase (FreeRoads 2) playerIndex'
      Knight -> do
        updateBonuses
        toSpecialPhase MovingRobber playerIndex'
      Invention -> toSpecialPhase Inventing playerIndex'
      Monopoly -> toSpecialPhase Monopolizing playerIndex'
      VictoryPoint -> return ()

updateBonuses :: (RandomGen g) => GameState g
updateBonuses = return () -- TODO

myFoldM :: Monad m => a -> [b] -> (a -> b -> m a) -> m a
myFoldM a1 lst f = foldM f a1 lst

doMonopoly :: (RandomGen g) => PlayerIndex -> Monopoly -> GameState g
doMonopoly playerIndex' (MonopolyOn resType) = do
  pIs <- uses players Map.keys
  newSum <- myFoldM mempty pIs $ \a b ->
    if b /= playerIndex'
    then do
      res <- resourcesOf b
      let res' = filteredResCount resType res
      addToResources b (mkNeg res')
      return $ a <> res'
    else return a
  addToResources playerIndex' newSum
  backToNormal

doInvention :: (RandomGen g) => PlayerIndex -> Invention -> GameState g
doInvention playerIndex' (InventionOf res) = do 
  addToResources playerIndex' res
  backToNormal

doMoveRobber :: (RandomGen g) => MoveRobber -> GameState g
doMoveRobber (MoveRobber dest) = do
  h <- use (board . hexes)
  let robberLoc = fromJust $ findKeyWhere _hasRobber h
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
  purchases' <- possiblePurchases
  trades <- possibleTradeActions
  cardsToPlay <- possibleDevelopmentCards
  validActions .= Set.unions [purchases', trades, cardsToPlay, Set.singleton (mkEndTurn currentPlayerIndex)]

possiblePurchases :: (RandomGen g) => GameStateReturning g (Set GameAction)
possiblePurchases = do
  currentPlayer' <- use currentPlayer 
  player' <- preuse (players.ix currentPlayer')
  b <- use board
  if isJust player'
  then do
    let player'' = fromJust player'
    let roads' = validRoadsFor (color player'') b
    let settlements' = validSettlementsFor (color player'') b
    let cities' = validCitiesFor (color player'') b
    let buildings' = Set.unions [roads', settlements', cities']
    let res = view resources player''
    let possibleBuildings = Set.filter (\x -> sufficient res (cost $ deconstruct x)) buildings'
    let cards' = Set.fromList [purchase currentPlayer' (Potential DevelopmentCard) | sufficient res (cost $ Potential DevelopmentCard)]
    let mkPurchase = purchase currentPlayer' . Building
    return $ Set.union cards' (Set.map mkPurchase possibleBuildings)
  else return Set.empty

simpleOffers :: (RandomGen g) => GameStateReturning g (Set GameAction)
simpleOffers = do
  currentPlayer' <- use currentPlayer
  pRes <- resourcesOf currentPlayer'
  let singles = resCombinationsForTotal 1
  let canOffer = filter (sufficient pRes) singles
  let mightWant = filter (not . sufficient pRes) singles
  let offers = mkOffer <$> [currentPlayer'] <*> canOffer <*> mightWant
  return $ Set.fromList offers

cannotReplyToTrade :: (RandomGen g) => GameStateReturning g (Set PlayerIndex)
cannotReplyToTrade = do
  openTrades' <- use openTrades
  currentPlayer' <- use currentPlayer
  let existingAccepts = Set.filter isAccept openTrades'
  let existingRejects = Set.filter isReject openTrades'
  let alreadyReplied = Set.union (mapSetMaybe (^?accepter) existingAccepts) (mapSetMaybe (^?rejecter) existingRejects)
  return $ Set.union (Set.singleton currentPlayer') alreadyReplied

openOffer :: (RandomGen g) => GameStateReturning g (Maybe TradeOffer)
openOffer = do
  openTrades' <- use openTrades
  return $ firstMaybe $ mapSetMaybe toOffer openTrades'

extantAccepts :: (RandomGen g) => GameStateReturning g (Set TradeAction)
extantAccepts = do
  openTrades' <- use openTrades
  return $ Set.filter isAccept openTrades'

possibleAccepts :: (RandomGen g) => GameStateReturning g (Set GameAction)
possibleAccepts = do
  offer' <- openOffer
  maybe (return Set.empty) mkAccept offer' 

otherPlayers :: (RandomGen g) => GameStateReturning g (Map.Map PlayerIndex Player)
otherPlayers = do
  current <- use currentPlayer
  uses players (Map.filter ((/= current) . view playerIndex))

mkAccept :: (RandomGen g) => TradeOffer -> GameStateReturning g (Set GameAction)
mkAccept offer' = do
  ps <- otherPlayers
  cannotReply <- cannotReplyToTrade
  let ps' = Map.filter (sufficient (offer'^.asking) . view resources) ps
  let couldAccept = Map.keysSet ps'
  return $ Set.map (accept offer') (couldAccept Set.\\ cannotReply)

possibleRejects :: (RandomGen g) => GameStateReturning g (Set GameAction)
possibleRejects = do
  offer' <- openOffer
  cannotReply <- cannotReplyToTrade
  otherIs <- liftM Map.keysSet otherPlayers
  let otherIs' = otherIs Set.\\ cannotReply
  let rejects = fmap (\x -> Set.map (reject x Nothing) otherIs') offer'
  return $ fromMaybe Set.empty rejects

originatedWith :: TradeOffer -> PlayerIndex -> Bool
originatedWith offer' playerIndex' = (offer'^.offeredBy) == playerIndex'

possibleCompletes :: (RandomGen g) => GameStateReturning g (Set GameAction)
possibleCompletes = do
  offer' <- openOffer
  if isJust offer'
  then do
    let offer'' = fromJust offer'
    currentPlayer' <- use currentPlayer
    extantAccepts' <- extantAccepts
    let canReply = originatedWith offer'' currentPlayer'
    completes' <- forM (Set.toList extantAccepts') $ \x -> do
      let accepter' = x ^?! accepter
      enoughResources <- checkResourcesForCompleteTrade offer'' accepter'
      return $ if enoughResources && canReply then complete x else Nothing
    return $ Set.fromList $ catMaybes completes'
  else return Set.empty

possibleTradeActions :: (RandomGen g) => GameStateReturning g (Set GameAction)
possibleTradeActions = do
  currentPlayer' <- use currentPlayer
  base <- simpleOffers
  offer' <- openOffer
  if isJust offer'
  then do
    let offer'' = fromJust offer'
    accepts' <- possibleAccepts
    rejects' <- possibleRejects
    completes' <- possibleCompletes
    let cancels = Set.fromList [cancel offer'' | offer''^.offeredBy == currentPlayer']
    return $ Set.unions [accepts', rejects', completes', cancels]
  else return base

possibleDevelopmentCards :: (RandomGen g) => GameStateReturning g (Set GameAction)
possibleDevelopmentCards = do
  currentPlayer' <- use currentPlayer
  items' <- inventory currentPlayer'
  let cards' = (Set.fromList . filter (/= VictoryPoint) . mapMaybe (^? card)) `fmap` items'
  return $ maybe Set.empty (Set.map (mkPlayCard currentPlayer')) cards'

canMoveRobberTo :: (RandomGen g) => GameStateReturning g (Set GameAction)
canMoveRobberTo = do
  currentPlayer' <- use currentPlayer
  robberSpots <- robbableSpots
  return $ Set.fromList $ map (mkMoveRobber currentPlayer') robberSpots

neighborBuildings :: (RandomGen g) => CentralPoint -> GameStateReturning g [OnPoint]
neighborBuildings cp = do
  b <- use board
  let buildings' = getHabitations b
  let myNeighbors = (Map.!) centersToNeighbors cp
  return $ mapMaybe (`Map.lookup` buildings') myNeighbors

robbableSpots :: (RandomGen g) => GameStateReturning g [CentralPoint]
robbableSpots = do
  currentPlayer' <- use currentPlayer
  ps <- use players >>= return . Map.elems
  ownColor <- preuses (players.ix currentPlayer') color
  if isJust ownColor
  then do
    let ownColor' = fromJust ownColor
    let playersColors = zip ps (map color ps)
    let protected = nub $ ownColor':(map snd $ filter (\(p,_) -> view displayScore p <= 2) playersColors)
    filterM (neighborBuildings >=> return . all (not . flip elem protected) . map color) hexCenterPoints
  else return []

-- | Called on EndTurn action or in initial phase
progress :: (RandomGen g) => GameState g
progress = do
  currentPlayerIndex' <- use currentPlayer
  let currentPlayerIndex = fromPlayerIndex currentPlayerIndex'
  adv <- use turnAdvanceBy
  totalPlayers <- uses players Map.size
  p <- use phase
  let next' = toPlayerIndex $ (totalPlayers + currentPlayerIndex + adv) `mod` totalPlayers
  let newAdv x | x == 3 && adv == 1 = 0
               | x == 3 && adv == 0 = -1
               | otherwise = adv
  nextPlayer <- uses players (Map.! next')
  nextActionsIfInitial <- uses board (possibleInitialSettlements nextPlayer)
  case p of
    Initial -> if currentPlayerIndex == 0 && adv == -1
               then do
                turnAdvanceBy .= 1
                initialToNormal
               else do
                 switchToPlayer next' nextActionsIfInitial
                 turnAdvanceBy .= newAdv (fromPlayerIndex next')
    Normal -> switchToPlayer next' (Set.singleton (rollFor next'))
    Special _ -> return () -- if progress called in a special phase, do nothing
    End -> return ()

switchToPlayer :: (RandomGen g) => PlayerIndex -> (Set GameAction) -> GameState g
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
  validActions .= Set.singleton (rollFor (toPlayerIndex 0))

backToNormal :: (RandomGen g) => GameState g
backToNormal = do
  phase .= Normal
  genPlayerActions

toSpecialPhase :: (RandomGen g) => SpecialPhase -> PlayerIndex -> GameState g
toSpecialPhase special' playerIndex' = do
  phase .= Special special'
  openTrades .= Set.empty
  case special' of
      FreeRoads _ -> do
        color' <- preuses (players.ix playerIndex') color
        when (isJust color') $ do
          let color'' = fromJust color'
          possibleRoads <- uses board (validRoadsFor color'')
          validActions .= Set.map (mkFree playerIndex') possibleRoads
      RobberAttack -> do -- ^ If no one has more than 7 resources, move on to MovingRobber.
        mustDiscard <- makeDiscards
        if not $ Set.null mustDiscard
        then validActions .= mustDiscard
        else toSpecialPhase MovingRobber playerIndex'
      MovingRobber -> do
        robberMoves <- canMoveRobberTo
        if Set.null robberMoves
        then backToNormal
        else validActions .= robberMoves
      Inventing -> validActions .= Set.map (invent playerIndex') possibleInventions
      Monopolizing -> validActions .= Set.map (PlayerAction playerIndex' . SpecialAction) possibleMonopolies

makeDiscards :: (RandomGen g) => GameStateReturning g (Set GameAction)
makeDiscards = do
  ps <- use players
  let pRes = Map.map (^. resources) ps
  let pMustDiscard = Map.filter ((> 7) . totalResources) pRes
  let listDiscards p r = let total' = totalResources r `div` 2
                             rs = resCombinationsForTotal total'
                         in  zip (repeat p) $ filter (sufficient r) rs
  let pDiscards = Set.fromList . concat . Map.elems $ Map.mapWithKey listDiscards pMustDiscard
  return $ Set.map mkDiscard pDiscards

-- | Game won by player p.
wonBy :: (RandomGen g) => PlayerIndex -> GameState g
wonBy pI = do
  phase .= End
  winner .= Just pI

-- | Choose a valid action at random (useful for testing purposes)
randomAct :: (RandomGen g) => GameState g
randomAct = do
  vA <- use validActions
  act' <- uniform (Set.toList vA)
  update act'

-- | Force a particular roll and update (useful for testing purposes).
forceRoll :: (RandomGen g) => Int -> GameState g
forceRoll i = do
  rolled .= Just i
  updateForRoll
