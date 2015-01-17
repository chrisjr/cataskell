module Cataskell.GameSpec (main, spec, toJS) where

import Test.Hspec
import Test.QuickCheck
import Cataskell.Game
import Cataskell.GameData.Actions
import Cataskell.GameData.Basics
import Cataskell.GameData.Board (roads)
import Cataskell.GameData.Player
import Cataskell.GameData.Resources
import Control.Monad.Random
import Control.Monad.Identity
import Control.Monad.State
import Data.List
import Data.Ord (comparing)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe
import Data.Monoid (mempty, (<>))
import Control.Applicative ((<$>), (<*>))
import Control.Lens hiding (elements)
import Data.Aeson (ToJSON, encode)
import qualified Data.ByteString.Lazy.Char8 as B
import Cataskell.Util
import Cataskell.Serialize()
import Cataskell.UtilSpec() -- for Arbitrary StdGen instance
import Cataskell.GameData.ActionsSpec()
import Cataskell.GameData.BasicsSpec()
import Cataskell.GameData.BoardSpec (blueRoadsLonger, whiteRoads)
import Cataskell.GameData.PlayerSpec()

newtype InitialGame = InitialGame Game
  deriving (Eq, Ord, Show, Read)

toInitialGame :: Game -> InitialGame
toInitialGame x | x ^. phase == Initial = InitialGame x
                | otherwise = error "Game phase is not initial"

fromInitialGame :: InitialGame -> Game
fromInitialGame (InitialGame x) = x

newtype NormalGame = NormalGame Game
  deriving (Eq, Ord, Show, Read)

toNormalGame :: Game -> NormalGame
toNormalGame x | x ^. phase == Normal = NormalGame x
               | otherwise = error "Game phase is not Normal"

fromNormalGame :: NormalGame -> Game
fromNormalGame (NormalGame x) = x

newtype TradeGame = TradeGame Game
  deriving (Eq, Ord, Show, Read)

toTradeGame :: Game -> TradeGame
toTradeGame x | x ^. phase == Normal && not (Set.null (x^.openTrades)) = TradeGame x
              | otherwise = error "No trades"

fromTradeGame :: TradeGame -> Game
fromTradeGame (TradeGame x) = x

newtype PurchaseGame = PurchaseGame Game
  deriving (Eq, Ord, Show, Read)

toPurchaseGame :: Game -> PurchaseGame
toPurchaseGame x | x ^. phase == Normal && not (Set.null (Set.filter (isJust . (^? action.item)) (x^.validActions))) = PurchaseGame x
                 | otherwise = error "No purchases"

fromPurchaseGame :: PurchaseGame -> Game
fromPurchaseGame (PurchaseGame x) = x

newtype EndGame = EndGame Game
  deriving (Eq, Ord, Show, Read)

toEndGame :: Game -> EndGame
toEndGame x | x ^. phase == End = EndGame x
            | otherwise = error "Not in end state"

fromEndGame :: EndGame -> Game
fromEndGame (EndGame x) = x

mkSteps :: (RandomGen g) => Rand g Int
mkSteps = getRandomR (0, 200)

mkGame :: (RandomGen g) => Rand g Game
mkGame = mkSteps >>= mkGame' Nothing

mkGame' :: (RandomGen g) => Maybe (GameState g) -> Int -> Rand g Game
mkGame' ra steps = do
  names <- uniform [["1", "2", "3"], ["1", "2", "3", "4"]]
  initialG <- newGame names
  let r = fromMaybe randomAct ra
  foldM (\acc _ -> execStateT r acc) initialG [0..steps]

mkGames :: (RandomGen g) => Rand g [Game]
mkGames = replicateM 100 mkGame

randomGames :: [Game]
randomGames = evalRand mkGames (mkStdGen 1)

instance (Arbitrary a, Ord a) => Arbitrary (Set a) where
  arbitrary = Set.fromList <$> arbitrary
  shrink s = Set.fromList <$> shrink (Set.toList s)

instance Arbitrary Game where
  arbitrary = elements randomGames
  shrink g = tail $ Game <$> [_phase g]
                    <*> shrink' (_board g)
                    <*> shrinkPlayers
                    <*> [_currentPlayer g]
                    <*> [0] -- turnadvance
                    <*> shrink' (_rolled g)
                    <*> shrink' (_validActions g)
                    <*> [_openTrades g]
                    <*> [_longestRoad g]
                    <*> [_largestArmy g]
                    <*> [Nothing] -- lastAction
                    <*> shrink' (_allCards g)
                    <*> shrink' (_winner g)
    where shrink' a = a : shrink a
          onlyReal = filter (isNothing . preview itemType)
          shrinkPlayers = let tradePs = Set.foldr (Set.union . playersReferencedInTrade) Set.empty $ Set.filter (not . isReject) (_openTrades g)
                              ps = Set.foldr (Set.union . playersReferenced) tradePs (_validActions g)
                              necessaryPlayers = Map.filterWithKey (\k _ -> k `Set.member` ps) (_players g)
                              needed = over (mapped.constructed) onlyReal necessaryPlayers
                          in  if Map.size (_players g) /= Map.size needed then [_players g, needed] else [needed]

instance Arbitrary NormalGame where
  arbitrary = NormalGame <$> elements (filter ((== Normal) . view phase) randomGames)
  shrink ng = map toNormalGame $ shrink $ fromNormalGame ng

instance Arbitrary TradeGame where
  arbitrary = toTradeGame <$> elements (filter (\g -> (g^.phase == Normal) && not (Set.null (g^.openTrades))) randomGames)
  shrink tg = map toTradeGame $ shrink $ fromTradeGame tg

instance Arbitrary PurchaseGame where
  arbitrary = toPurchaseGame <$> elements 
    (filter (\g -> (g^.phase == Normal) && not (Set.null (Set.filter (isJust . (^? action.item)) (g^.validActions)))) randomGames)
  shrink pg = map toPurchaseGame $ shrink $ fromPurchaseGame pg

instance Arbitrary InitialGame where
  arbitrary = do
    stdGen <- arbitrary :: Gen StdGen
    names <- elements [["1", "2", "3"], ["1", "2", "3", "4"]]
    return $ toInitialGame $ evalRand (newGame names) stdGen

lateGame :: StdGen -> Gen Game
lateGame stdGen = sized $ \n ->
  do k <- choose (0, n) -- around 2500?
     return $ evalRand (mkGame' (Just randomActGoodInitial) k) stdGen

newtype LateGame = LateGame Game

toLateGame :: Game -> LateGame
toLateGame x | hasHighScorer x = LateGame x
             | otherwise = error "Not high enough scores"

fromLateGame :: LateGame -> Game
fromLateGame (LateGame game) = game

dummyRand :: StdGen
dummyRand = mkStdGen 0

hasHighScorer :: Game -> Bool
hasHighScorer g = let scores' = evalGame otherDisplayScores g dummyRand
                  in (g^.phase == Normal) && any (>2) (Map.elems scores')

instance Arbitrary LateGame where
  arbitrary = toLateGame <$> elements (filter hasHighScorer randomGames)

instance Arbitrary EndGame where
  arbitrary = do
    stdGen <- arbitrary :: Gen StdGen
    game <- lateGame stdGen `suchThat` ((== End) . view phase)
    return $ toEndGame game

-- newtype GameStateStd = GameStateStd (StateT Game (RandT StdGen Identity) ())

toJS :: (ToJSON a, Testable prop) => a -> prop -> Property
toJS a = counterexample ("\nvar state=" ++ B.unpack (encode a) ++ "; showState(state);")

p0 :: PlayerIndex
p0 = toPlayerIndex 0

p1 :: PlayerIndex
p1 = toPlayerIndex 1

p2 :: PlayerIndex
p2 = toPlayerIndex 2

p3 :: PlayerIndex
p3 = toPlayerIndex 3

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
  gameInvariantSpec
  gameStateReturningSpec
  sampleGameSpec

gameInvariantSpec :: Spec
gameInvariantSpec = parallel $ do
  describe "A new game" $ do
    it "should start in the Initial phase" $ property $
      \g -> view phase (fromInitialGame (g :: InitialGame)) == Initial
    it "must have either 3 or 4 players" $ property $
      \game -> let l = Map.size $ view players $ fromInitialGame (game :: InitialGame)
               in l == 3 || l == 4
    it "should allow for the retrieval of a specific player" $ property $
      \game stdGen -> let g' = fromInitialGame (game :: InitialGame)
                          i' = evalRand (evalStateT (findPlayerByColor Blue) g') (stdGen :: StdGen)
                          i = fromPlayerIndex i'
                          in i >= 0 && i < 4
    it "after (# players) * 4 updates, must be in Normal phase" $ property $
      \game -> let g = fromInitialGame game
                   l = Map.size $ view players g
                   normalTurn = l * 4
                   gs = iterate (uncurry (runGame randomAct)) (g, dummyRand)
               in (gs !! normalTurn)^._1.phase == Normal
  describe "Game invariants" $ do
    it "has a non-zero list of next actions, unless at the end" $ property $
      \game -> let n = (game :: Game) ^.validActions.to Set.size
               in (n > 0) || (view phase game == End)
    it "has only valid actions in the validActions list" $ property $
      \(Blind game) stdGen -> let vA = (game :: Game) ^. validActions
                                  allExist = allS (`playersExistFor'` game) vA
                                  isValid' x = evalRand (evalStateT (isValid x) game) (stdGen :: StdGen)
                              in allExist ==> toJS game $ allS isValid' vA
    it "has at most one player with >= 10 victory points" $ property $
      \game -> let scores' = evalGame scores game (mkStdGen 0)
                   highestCount = Map.findMax $ counts scores'
               in  (fst highestCount < 10) || (snd highestCount == 1)
    it "should have only non-negative resource counts" $ property $
      \game -> let res' = evalGame (liftM (Map.map (^.resources)) (use players)) game (mkStdGen 0)
               in  all nonNegative $ Map.elems res'
    it "should deduct resources when a valid purchase is made" $ property $
      \(Blind pg) -> 
       let game = fromPurchaseGame pg
           vA = game ^. validActions
           purchase' = findS (isJust . preview (action.item.building)) vA
           allExist = allS (`playersExistFor'` game) vA
           pI = game ^. currentPlayer
           bldg = purchase' >>= preview (action.item.building)
       in isJust bldg && allExist ==>
         let oldRes = game ^. players . ix pI . resources
             purchase'' = fromJust purchase'
             bldg' = fromJust bldg
             (g', _) = runGame (update purchase'') game (mkStdGen 0)
             newRes = g' ^. players . ix pI . resources
         in toJS game $ newRes === oldRes <> mkNeg (cost (Building bldg'))

getFromGame :: GameStateReturning StdGen a -> Game -> a
getFromGame x g = evalGame x g dummyRand

actionsFromGame :: Game -> Set GameAction
actionsFromGame = getFromGame (use validActions)

checkIfValidOffer :: Game -> Bool
checkIfValidOffer g = let offer' = firstMaybe $ mapSetMaybe toOffer $ tradesFrom g 
                          oA = fmap (\x -> (x^.offeredBy, x^.asking)) offer'
                          couldRespond = join $ fmap (\(o, x) -> findKeyValueWhere (\pI p -> pI /= o && sufficient (p^.resources) x) (g^.players)) oA
                      in isJust couldRespond

tradesFrom :: Game -> Set TradeAction
tradesFrom = getFromGame (use openTrades)

checkIfOpenTrade :: (TradeAction -> Bool) -> Game -> Bool
checkIfOpenTrade cImplies = anyS cImplies . tradesFrom

checkForPossibleTrade :: (TradeAction -> Bool) -> Game -> Bool
checkForPossibleTrade x game' = let acts' = actionsFromGame (game' :: Game)
                                in  anyS ((== Just True) . fmap x . preview (action.trade)) acts'

gameStateReturningSpec :: Spec
gameStateReturningSpec =
  describe "GameStateReturning functions" $ do
    let allValid f (Blind ng) = let g = fromNormalGame ng
                                    r' = mkStdGen 0
                                in toJS g $ and $ evalGame (f >>= \x -> mapM isValid (Set.toList x)) g r'
    context "simpleOffers" $ do
      it "should produce only offers that the offerer can fulfill" $ property $
        \(Blind tg) -> let g = fromTradeGame tg
                           openTrades' = view openTrades g
                           offer' = firstMaybe $ mapSetMaybe toOffer openTrades'
                       in isJust offer' ==>
                         let offer'' = fromJust offer'
                             pI = offer''^.offeredBy
                             pRes = view (players . ix pI . resources) g
                         in sufficient pRes (offer''^.offering)
      it "should not generate invalid actions" $ property $
        allValid simpleOffers
    context "simpleExchanges" $
      it "should not generate invalid actions" $ property $
        allValid simpleExchanges
    context "possiblePurchases" $
      it "should not generate invalid actions" $ property $
        allValid possiblePurchases
    context "possibleDevelopmentCards" $
      it "should not generate invalid actions" $ property $
        allValid possibleDevelopmentCards
    context "otherPlayers" $
      it "should return a map excluding the current player" $ property $
        \(Blind g) -> let current = g^.currentPlayer
                          others = Map.keysSet $ getFromGame otherPlayers g
                      in toJS g $ others == Map.keysSet (g^.players) Set.\\ Set.singleton current
    
    let getHasReplied g = let openTrades' = g^.openTrades
                              offer' = firstMaybe $ mapSetMaybe toOffer openTrades'
                              hasRepliedAlready = Set.filter (\x -> isReject x || isAccept x) openTrades'
                          in (offer', hasRepliedAlready)
    context "possibleAccepts" $
      it "should generate accepts when an offer is present and others can fulfill it" $ property $
        \(Blind tg) -> let g = fromTradeGame tg
                           (_, hasRepliedAlready) = getHasReplied g 
                       in checkIfValidOffer g && Set.null hasRepliedAlready ==> 
                         toJS g $ checkForPossibleTrade isAccept g
    context "possibleRejects" $
      it "should generate rejects when an offer is present, for all except offerer" $ property $ 
        \(Blind tg) -> let g = fromTradeGame tg
                           (offer', hasRepliedAlready) = getHasReplied g
                       in Set.null hasRepliedAlready && isJust offer' ==> 
                         let stdGen = mkStdGen 0
                             rejects'' = evalGame possibleRejects g stdGen
                             otherPlayers' = Map.keysSet (g^.players) Set.\\ Set.singleton (g^.currentPlayer)
                         in toJS g $ otherPlayers' == mapSetMaybe (^?action.trade.rejecter) rejects''
    context "possibleCompletes" $
      it "should always generate a complete when accepts are present" $ property $
        \(Blind tg) -> let g = fromTradeGame tg
                           (_, hasRepliedAlready) = getHasReplied g
                           accept' = firstMaybe $ Set.filter (\a -> case a of (PlayerAction _ (Trade x)) -> isAccept x; _ -> False) (g^.validActions)
                       in checkIfValidOffer g && Set.null hasRepliedAlready && isJust accept' ==> 
                         let accept'' = fromJust accept'
                             g' = execGame (update accept'') g dummyRand
                         in toJS g' $ not (Set.null $ Set.filter (\a -> case a of (PlayerAction _ (Trade x)) -> isComplete x; _ -> False) (g'^.validActions))
    let game = head $ filter ((== Normal) . (^.phase)) randomGames
    context "possibleTradeActions" $ do
      let rand = mkStdGen 0
      let offer' = TradeOffer mempty {ore = 1} mempty {wheat = 1} p0
      let accept' = accept offer' p1
      let complete' = fromJust $ complete $ accept'^?!action.trade
      let setAll = [ set (players . ix p0. resources) mempty { ore = 1 }
                   , set (players . ix p1. resources) mempty { wheat = 1 }
                   , set (players . ix p2 . resources) mempty
                   , set openTrades Set.empty
                   , set currentPlayer p0
                   ]
      let preOfferGame = foldr (.) id setAll game
      let offerGame = execGame (update PlayerAction { _actor = p0, _action = Trade (Offer offer')}) preOfferGame rand
      let offerAndAcceptGame = execGame (update accept') offerGame rand
      it "should generate an accept when offer is present and another can fulfill" $
        actionsFromGame offerGame `shouldSatisfy` Set.member accept'
      it "should generate a complete when offer and acceptance are present" $
        actionsFromGame offerAndAcceptGame `shouldSatisfy` Set.member complete'
      it "should never have an accept and reject by the same player" $ property $
        \(Blind tg) -> 
          let g = fromTradeGame tg
              trades' = tradesFrom g
              rejecters = Set.map (^?!rejecter) $ Set.filter isReject trades'
              accepters = Set.map (^?!accepter) $ Set.filter isAccept trades'
          in  toJS g $ Set.null (accepters `Set.intersection` rejecters)
      it "should never have an offer and an accept/reject by the same player" $ property $
        \(Blind tg) -> 
          let g = fromTradeGame tg
              trades' = tradesFrom g
              offerer = mapSetMaybe (^?offer.offeredBy) trades'
              rejecters = Set.map (^?!rejecter) $ Set.filter isReject trades'
              accepters = Set.map (^?!accepter) $ Set.filter isAccept trades'
          in  toJS g $ Set.null (offerer `Set.intersection` (accepters `Set.union` rejecters))
      it "should never produce invalid actions" $ property $
        allValid possibleTradeActions
    context "getCard" $
      it "should add a card to a player's newCards field" $ property $
        \(Blind ng) -> let g = fromNormalGame ng
                           topcard = views allCards listToMaybe g
                           c' = fmap Card topcard
                           pI = view currentPlayer g
                           hasNewCard x = fmap (`elem` view (players . ix pI . newCards) x) topcard
                           hasCard x = fmap (`elem` view (players . ix pI . constructed) x) c'
                           vA = view validActions g
                           endTurn = findS ((== EndTurn) . view action) vA
                       in hasNewCard g == Just False && isJust endTurn ==> 
                         let r = mkStdGen 0
                             g' = execGame getCard g r
                             f end = let afterEndTurn = execGame (update end) g' r
                                     in hasNewCard afterEndTurn == Just False && hasCard afterEndTurn == Just True
                             doesntKeep = maybe False f endTurn
                         in toJS g' $ hasNewCard g' == Just True && doesntKeep
    context "makeDiscards" $
      it "should generate discards for players with >7 resources" $ do
        let setAll = [ set (players . ix p0 . resources) mempty { ore = 8 }
                     , set (players . ix p1 . resources) mempty { lumber = 9 }
                     , set (players . ix p2 . resources) mempty { brick = 3 }
                     , set (players . ix p3 . resources) mempty
                     ]
        let game' = foldr (.) id setAll game
        let discards = evalGame makeDiscards game' (mkStdGen 0)
        discards `shouldBe` Set.fromList [ mkDiscard (p0, mempty { ore = 4 })
                                         , mkDiscard (p1, mempty { lumber = 4 })]
    context "robbableSpots" $ do
      let r = mkStdGen 0
      it "should return an unoccupied hex (or one occupied by current player) if no enemy player has >2 visible victory points" $ property $
        \ng -> let g = fromNormalGame ng
                   others = Map.keysSet $ Map.filter (>2) $ evalGame otherDisplayScores g r
               in Set.null others ==>
                 let spots = evalGame robbableSpots g r
                     currentPlayer' = view currentPlayer g
                     myColor = color $ fromJust (g^?players.ix currentPlayer')
                 in  not (null spots) && all (\hc -> all ((== myColor) . color) $ evalGame (neighborBuildings hc) g r) spots
      it "should return a hex surrounded by other players with >2 visible victory points, if one exists" $ property $
        \(Blind lg) -> let g = fromLateGame lg
                           others = Map.keysSet $ Map.filter (>2) $ evalGame otherDisplayScores g r
                       in not (Set.null others) ==>
                         let spots = evalGame robbableSpots g r
                             players' = view players g
                             vulnerable = Map.elems . Map.map color $ Map.filterWithKey (\k _ -> k `Set.member` others) players'
                         in not (null spots) && all (\hc -> all (flip elem vulnerable . color) $ evalGame (neighborBuildings hc) g r) spots

sampleGameSpec :: Spec
sampleGameSpec = parallel $ do
  describe "An example game" $ do
    let (initialGame, r') = runRand (newGame ["1", "2", "3", "4"]) (mkStdGen 0)
    let gs = iterate (uncurry (runGame randomAct)) (initialGame, r')
    let (normalGame, _) = gs !! 16

    context "in Initial phase" $ do
      it "should initially allow a settlement built anywhere" $ do
        let valids = view validActions initialGame
        Set.size valids `shouldBe` 54
      context "when cycling through players forward then back" $ do
        let playerIs = [0, 0, 1, 1, 2, 2, 3, 3, 3, 3, 2, 2, 1, 1, 0, 0]

        forM_ (zip3 [0..] playerIs (map fst gs)) $ \(i, c, g) ->
          it ("should permit placement for player " ++ show c) $ do
            view currentPlayer g `shouldBe` toPlayerIndex c
            views validActions Set.size g `shouldSatisfy` (<= (54 - ((i `div` 2) * 3)))
      it "should transition to Normal phase when placements are complete" $ do
        view phase normalGame `shouldBe` Normal
        views validActions Set.size normalGame `shouldBe` 1

    -- produce some variants that will be used later

    let (rolledOnce, randRolled) = gs !! 17

    let withRoadBuilding = (players . ix p0 . constructed) <>~ [Card RoadBuilding] $ rolledOnce
    let withInvention = (players . ix p0 . constructed) <>~ [Card Invention] $ rolledOnce
    let withMonopoly = (players . ix p0 . constructed) <>~ [Card Monopoly] $ rolledOnce
    let withKnight = (players . ix p0 . constructed) <>~ [Card Knight] $ rolledOnce
    let rolled7 = rolled .~ Just 7 $ rolledOnce

    context "in Normal phase" $ do
      it "should start each turn with a roll" $ do
        let vA = view validActions normalGame
        vA `shouldBe` Set.singleton (rollFor p0)
        view turnAdvanceBy normalGame `shouldBe` 1

      it "should distribute resources once a roll happens" $ do
        let totalAsOf g = sum . Map.elems . Map.map (views resources totalResources) $ view players g
        let (starting, _)  = gs !! 16
        totalAsOf rolledOnce `shouldSatisfy` (> (totalAsOf starting))

      it "should allow for building roads once resources are sufficient" $ do
        let findWoodBrickPlayer g = findKeyWhere (\p -> sufficient (p^.resources) (cost $ unbuilt road)) (g^.players)
        let findSuitable (g, _) = let p' = findWoodBrickPlayer g
                                      isCurrent = fmap (== g^.currentPlayer) p'
                                      r' = fmap rollFor p'
                                      hasRolled = fmap (`Set.notMember` (view validActions g)) r'
                                  in ((g^.phase) == Normal) && isCurrent == Just True && hasRolled == Just True
        let (g', _) = fromJust $ find findSuitable gs
        let isRoadPurchase a = maybe False (\item' -> isJust $ item' ^? building.onEdge) (a^? (action.item))
        view validActions g' `shouldSatisfy` anyS isRoadPurchase

      let pOffer = mkOffer (view currentPlayer rolledOnce) (mempty { ore = 1} ) (mempty { wheat = 1 } )

      it "should allow for a trade offer" $ do
        let vA = view validActions rolledOnce
        vA `shouldSatisfy` (anyS isTradeAction)

      context "when trading" $ do
        let playerWithOre g = let ps = view players g
                              in findKeyWhere (\p -> (ore $ view resources p) >= 1) ps
        let playerWithWheat g = let ps = view players g
                                in findKeyWhere (\p -> (wheat $ view resources p) >= 1) ps

        let wellResourced g = let o = playerWithOre g
                                  w = playerWithWheat g
                                  c = if isJust o
                                      then let p = fromJust o
                                           in  p == (view currentPlayer g)
                                      else False
                                  d = Normal == view phase g
                                  e = anyS isTradeAction (view validActions g)
                              in and [isJust o, isJust w, c, d, e]
        let maybeFurtherAlong = find (\(g, _) -> wellResourced g) gs
        let (furtherAlong, _) = fromJust maybeFurtherAlong

        let pI = view currentPlayer furtherAlong
        let pOffer' = mkOffer pI (mempty { ore = 1} ) (mempty { wheat = 1 } )
        let tradeOffer' = fromJust $ pOffer ^? action.trade.offer

        let beforeTradeP1 = furtherAlong^.players.ix pI.resources

        let g' = execGame (update pOffer') furtherAlong dummyRand
        let others = views players (Map.filterWithKey (\k _ -> k /= pI)) g'
        let otherIs = Map.keysSet others
        let acceptancesAll = Set.map (accept tradeOffer') otherIs
        let validAcceptance g accept' = let thisPI = accept'^?!action.trade.accepter
                                            res = accept'^?!action.trade.offer.asking
                                        in  sufficient (g^.players.ix thisPI.resources) res
        let acceptances = Set.filter (validAcceptance g') acceptancesAll
        let rejections = Set.map (reject tradeOffer' Nothing) otherIs

        it "should add an offer to the open trades" $
          view openTrades g' `shouldBe` Set.singleton (Offer tradeOffer')
        it "should allow others to accept or reject" $ do
          let vA = Set.filter isTradeAction $ view validActions g'
          vA `shouldSatisfy` (\x -> acceptances `Set.isSubsetOf` x)
          vA `shouldSatisfy` (\x -> rejections `Set.isSubsetOf` x)
        it "should allow the original player to cancel" $ do
          let vA = view validActions g'
          let cancel' = cancel tradeOffer'
          vA `shouldSatisfy` Set.member cancel'

        let acceptance = head $ Set.toList acceptances
        let accepted = execGame (update acceptance) g' dummyRand

        let acceptance' = acceptance^?!action.trade
        let acceptedOffer = Offer (acceptance'^.offer)
        let aPI = acceptance^?!actor

        let beforeTradeP2 = furtherAlong^.players.ix aPI.resources
        let complete' = fromJust $ complete acceptance'

        it "should let another player accept" $ do
          view openTrades accepted `shouldBe` Set.fromList [acceptedOffer, acceptance']
          let pta = getFromGame possibleTradeActions accepted
          pta `shouldSatisfy` Set.member complete'
          let vA = Set.filter isTradeAction $ view validActions accepted
          vA `shouldSatisfy` Set.member complete'

        let completed = execGame (update complete') accepted dummyRand
        let afterTradeP1 = completed^.players.ix pI.resources
        let afterTradeP2 = completed^.players.ix aPI.resources

        it "should let the first player complete the trade" $ do
          view openTrades completed `shouldSatisfy` Set.null
          ore beforeTradeP1 `shouldSatisfy` (> (ore afterTradeP1))
          ore beforeTradeP2 `shouldSatisfy` (< (ore afterTradeP2))
          wheat beforeTradeP1 `shouldSatisfy` (< (wheat afterTradeP1))
          wheat beforeTradeP2 `shouldSatisfy` (> (wheat afterTradeP2))
      it "should transition to Special RobberAttack when a 7 is rolled" $ do
        let robbed' = execGame updateForRoll rolled7 dummyRand
        view phase robbed' `shouldBe` Special MovingRobber
      it "should transition to Special RobberAttack when a Knight is played" $ do
        let playCard' = mkPlayCard (toPlayerIndex 0) Knight
        let g' = execGame (update playCard') withKnight dummyRand
        view phase g' `shouldBe` Special MovingRobber
      it "should transition to Special FreeRoads when a RoadBuilding card is played" $ do
        let playCard' = mkPlayCard (toPlayerIndex 0) RoadBuilding
        let g' = execGame (update playCard') withRoadBuilding dummyRand
        view phase g' `shouldBe` Special (FreeRoads 2)
      it "should transition to Special Inventing when an Invention card is played" $ do
        let playCard' = mkPlayCard (toPlayerIndex 0) Invention
        let g' = execGame (update playCard') withInvention dummyRand
        view phase g' `shouldBe` Special Inventing
      it "should transition to Special Monopolizing when a Monopoly card is played" $ do
        let playCard' = mkPlayCard (toPlayerIndex 0) Monopoly
        let g' = execGame (update playCard') withMonopoly dummyRand
        view phase g' `shouldBe` Special Monopolizing

    context "in the Special phase" $ do
      context "RobberAttack" $ do
        let setAll = [ set (players . ix p0 . resources) mempty { ore = 8 }
                     , set (players . ix p1 . resources) mempty { wheat = 10 }
                     , set (players . ix p2 . resources) mempty { brick = 7 }
                     , set rolled $ Just 7
                     ]
        let toRobGame = (foldr (.) id setAll) rolledOnce
        let robbedGame = execGame updateForRoll toRobGame dummyRand
        let disc1 = mkDiscard (p0, mempty { ore = 4 })
        let disc2 = mkDiscard (p1, mempty { wheat = 5 })
        it "should force any players with over 7 resources to discard half" $ do
          view phase robbedGame `shouldBe` Special RobberAttack
          let vA = view validActions robbedGame
          vA `shouldBe` Set.fromList [disc1, disc2]
        it "should transition into MovingRobber phase after discards" $ do
          let postDiscard0 = execGame (update disc1) robbedGame dummyRand
          let postDiscard = execGame (update disc2) postDiscard0 dummyRand
          view phase postDiscard `shouldBe` Special MovingRobber
      context "MovingRobber" $
        describe "the player must move the robber" $ do
          let getG x = if x^.phase == Special MovingRobber
                          then x
                          else getG (execGame randomAct x dummyRand)
          let isValidRobberMove g a = let pScores = Map.map (view displayScore) $ view players g
                                          pColors = Map.map color $ view players g
                                          high = Map.filter (>2) pScores
                                          acceptableColors = Map.elems $ Map.intersection pColors high
                                          neighbors' dest = evalGame (neighborBuildings dest) g dummyRand
                                          checkDest dest = not (null (neighbors' dest)) && all (flip elem acceptableColors . color) (neighbors' dest)
                                      in case a^.action of
                                           SpecialAction (MR (MoveRobber dest)) -> checkDest dest
                                           _ -> False
          let allValidRobberMove g = allS (isValidRobberMove g) (view validActions g)
          context "when someone has >2 visible victory points" $ do
            specify "to a hex ringed by players with >2 visible victory points" $ property $
              \(Blind lg) -> let g = fromLateGame lg
                                 scores' = Map.elems $ getFromGame otherDisplayScores g
                             in any (>2) scores' ==>
                                  let g7 = execGame (forceRoll 7) g dummyRand
                                      g' = getG g7
                                  in toJS g' $ allValidRobberMove g'
            specify "and should rob one of the players there" $ property $
              \(Blind lg) -> let g = fromLateGame lg
                                 scores' = Map.elems $ getFromGame otherDisplayScores g
                                 gR = if any (>2) scores'
                                        then let g7 = execGame (forceRoll 7) g dummyRand
                                                 gPost7 = getG g7
                                             in if allValidRobberMove gPost7 then Just (execGame randomAct gPost7 dummyRand) else Nothing
                                        else Nothing 
                             in isJust gR && fmap (anyS isRob . view validActions) gR == Just True ==>
                                let g' = fromJust gR
                                    robAction = head (Set.toList (view validActions g'))
                                    p1' = view currentPlayer g'
                                    p2' = fromJust $ robAction^?action.specialAction.robbed
                                    ps = view players g'
                                    g'' = execGame (update robAction) g' dummyRand
                                    ps' = view players g''
                                    beforeRobP1 = totalResources $ view resources (ps Map.! p1')
                                    beforeRobP2 = totalResources $ view resources (ps Map.! p2')
                                    afterRobP1 = totalResources $ view resources (ps' Map.! p1')
                                    afterRobP2 = totalResources $ view resources (ps' Map.! p2')
                                in (beforeRobP1 + 1) == afterRobP1 && beforeRobP2 == (afterRobP2 + 1)
          specify "to an unoccupied hex, otherwise" $ property $
            \(Blind ng) -> 
               let g = fromNormalGame ng
                   scores' = Map.elems $ getFromGame otherDisplayScores g
               in not (any (>2) scores') ==>
                       let g7 = execGame (forceRoll 7) g dummyRand
                           g' = getG g7
                           vA = view validActions g'
                           checkDest dest = null $ evalGame (neighborBuildings dest) g' dummyRand
                           isValidRobberMove a = case a^.action of
                                                   SpecialAction (MR (MoveRobber dest)) -> checkDest dest
                                                   _ -> False
                       in  toJS g' $ allS isValidRobberMove vA
      context "FreeRoads" $ do
        let playCard' = mkPlayCard (toPlayerIndex 0) RoadBuilding
        let (g', _) = runGame (update playCard') withRoadBuilding dummyRand
        let pI = view currentPlayer g'
        let isFreeRoad x = isJust $ x ^? action.construct.onEdge

        let (built1, _) = runGame randomAct g' dummyRand
        let (built2, _) = runGame randomAct built1 dummyRand

        it "should allow the current player to build two roads" $ do
          view validActions g' `shouldSatisfy` allS isFreeRoad
          view validActions built1 `shouldSatisfy` allS isFreeRoad
        it "should return to normal after two roads are built" $ do
          let vA = view validActions built2
          vA `shouldSatisfy` (not . anyS isFreeRoad)
          vA `shouldSatisfy` Set.member (mkEndTurn pI)
          view phase built2 `shouldBe` Normal
      context "Inventing" $
        it "should allow the current player to obtain two resources of their choice" $ do
          let resPre = view (players . ix p0 . resources) withInvention
          let lumberPre = lumber resPre
          let wheatPre = wheat resPre
          let playCard' = mkPlayCard p0 Invention
          let (g', _) = runGame (update playCard') withInvention dummyRand
          let inventions = Set.map (invent p0) possibleInventions
          view validActions g' `shouldBe` inventions
          let lumberWheatInvention = PlayerAction p0 (SpecialAction (I (InventionOf mempty { lumber = 1, wheat = 1})))
          let (postLumberWheatInvention, _) = runGame (update lumberWheatInvention) g' dummyRand

          let resPost = view (players . ix p0 . resources) postLumberWheatInvention
          let lumberPost = lumber resPost
          let wheatPost = wheat resPost
          lumberPost `shouldSatisfy` (== lumberPre + 1)
          wheatPost `shouldSatisfy` (== wheatPre + 1)
      context "Monopolizing" $
        it "should allow the current player to obtain all resources of a certain type" $ do
          let playCard' = mkPlayCard p0 Monopoly
          let (g', _) = runGame (update playCard') withMonopoly dummyRand
          let monopolies = Set.map (PlayerAction p0 . SpecialAction) possibleMonopolies
          view validActions g' `shouldBe` monopolies

          let lumberMonopoly = PlayerAction p0 (SpecialAction (M (MonopolyOn Lumber)))
          let (postMonopoly, _) = runGame (update lumberMonopoly) g' dummyRand

          let pRes = Map.elems . Map.map (view resources) $ postMonopoly^.players
          let totalLumber = sum $ map lumber pRes
          pRes `shouldSatisfy` all (\r -> lumber r == 0 || lumber r == totalLumber)
    context "in End phase" $
      it "has no more validActions" $ property $
        \(Blind lg) -> let g = fromLateGame lg
                           pI = view currentPlayer g
                           endTurn = findS (\(PlayerAction _ act') -> act' == EndTurn) (view validActions g)
                           g' = g & players . ix pI . constructed <>~ replicate 8 (Card VictoryPoint)
                           g'' = fmap (\end -> execGame (update end) g' dummyRand) endTurn
                       in isJust g'' ==> 
                         let finalG = fromJust g''
                         in toJS finalG $ finalG^.winner == Just pI && Set.null (view validActions finalG)
  describe "updateBonuses" $ do
    let urGame = evalRand (newGame (replicate 4 "")) dummyRand
    let getBonuses pI = view (players . ix pI . bonuses)
    context "when comparing roads" $ do
      let g = urGame & board.roads .~ blueRoadsLonger
      let g' = execGame updateBonuses g dummyRand
      let colorToPlayer = Map.fromList . map (\(k,v) -> (color v, k)) $ Map.toList (g^.players)
      let pI = colorToPlayer Map.! Blue
      it "should grant the LongestRoad when the first player has a road of length 5" $ do
        g^.longestRoad `shouldBe` Nothing
        g'^.longestRoad `shouldBe` Just (pI, 5)
        getBonuses pI g' `shouldBe` Set.singleton LongestRoad
      it "should transfer the LongestRoad when a player's road has been surpassed" $ do
        let pI' = colorToPlayer Map.! White
        let g'' = (board.roads) <>~ whiteRoads $ g'
        let updated = execGame updateBonuses g'' dummyRand
        updated^.longestRoad `shouldBe` Just (pI', 6)
        getBonuses pI updated `shouldSatisfy` Set.null
        getBonuses pI' updated `shouldBe` Set.singleton LongestRoad
    context "when comparing armies" $ do
      let g = urGame & players . ix p0 . knights .~ 3
      let g' = execGame updateBonuses g dummyRand
      it "should grant the LargestArmy when the first player has an army of size 3" $ do
        let gl = g' ^. largestArmy
        gl `shouldBe` Just (p0, 3)
        getBonuses p0 g' `shouldBe` Set.singleton LargestArmy
        getBonuses p1 g' `shouldSatisfy` Set.null
      it "should leave the bonus unchanged when another player meets that size" $ do
        let g'' = g' & players . ix p1 . knights .~ 3
        let g''' = execGame updateBonuses g'' dummyRand
        let gl = g''' ^. largestArmy
        gl `shouldBe` Just (p0, 3)
        getBonuses p0 g''' `shouldBe` Set.singleton LargestArmy
        getBonuses p1 g''' `shouldSatisfy` Set.null
      it "should transfer the LargestArmy when a player's army has been surpassed" $ do
        let g'' = g' & players . ix p1 . knights .~ 4
        let g''' = execGame updateBonuses g'' dummyRand
        let gl = g''' ^. largestArmy
        gl `shouldBe` Just (p1, 4)
        getBonuses p0 g''' `shouldSatisfy` Set.null
        getBonuses p1 g''' `shouldBe` Set.singleton LargestArmy
