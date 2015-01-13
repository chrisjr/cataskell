module Cataskell.GameSpec (main, spec, toJS) where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Modifiers (Blind)
import Cataskell.Game
import Cataskell.GameData.Actions
import Cataskell.GameData.Basics
import Cataskell.GameData.Player
import Cataskell.GameData.Resources
import Control.Monad.Random
import Control.Monad.Identity
import Control.Monad.State
import Data.List
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (fromJust, isJust, isNothing, mapMaybe)
import Data.Monoid (mempty, (<>))
import Control.Applicative ((<$>), (<*>))
import Control.Lens hiding (elements)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B
import Cataskell.Util
import Cataskell.Serialize()
import Cataskell.UtilSpec() -- for Arbitrary StdGen instance
import Cataskell.GameData.ActionsSpec()
import Cataskell.GameData.BasicsSpec()
import Cataskell.GameData.BoardSpec()
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

mkSteps :: (RandomGen g) => Rand g Int
mkSteps = getRandomR (0, 500)

mkGame :: (RandomGen g) => Rand g Game
mkGame = do
  names <- uniform [["1", "2", "3"], ["1", "2", "3", "4"]]
  initialG <- newGame names
  steps <- mkSteps
  foldM (\acc _ -> (execStateT randomAct) acc) initialG [0..steps]

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

instance Arbitrary InitialGame where
  arbitrary = do
    stdGen <- arbitrary :: Gen StdGen
    names <- elements [["1", "2", "3"], ["1", "2", "3", "4"]]
    return $ toInitialGame $ evalRand (newGame names) stdGen

-- newtype GameStateStd = GameStateStd (StateT Game (RandT StdGen Identity) ())

toJS :: (ToJSON a, Testable prop) => a -> prop -> Property
toJS a = counterexample ("\nvar state=" ++ B.unpack (encode a) ++ "; showState(state);")

p0 :: PlayerIndex
p0 = toPlayerIndex 0

p1 :: PlayerIndex
p1 = toPlayerIndex 1

p2 :: PlayerIndex
p2 = toPlayerIndex 2

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  gameInvariantSpec
  gameStateReturningSpec
  sampleGameSpec

gameInvariantSpec :: Spec
gameInvariantSpec = do
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
      \(Blind ng) pI -> 
       let game = fromNormalGame ng
           vA = game ^. validActions
           purchase' = findS (isJust . preview (action.item.building)) vA
           allExist = allS (`playersExistFor'` game) vA
           bldg = purchase' >>= preview (action.item.building)
       in isJust bldg && allExist ==>
         let oldRes = game ^. players . ix pI . resources
             purchase'' = fromJust purchase'
             bldg' = fromJust bldg
             (g', _) = runGame (update purchase'') game (mkStdGen 0)
             newRes = g' ^. players . ix pI . resources
         in toJS game $ newRes === oldRes <> mkNeg (cost (Building bldg'))

gameStateReturningSpec :: Spec
gameStateReturningSpec =
  describe "GameStateReturning functions" $ do
    let allValid f (Blind ng) = let g = fromNormalGame ng
                                    pI = view currentPlayer g
                                    r' = mkStdGen 0
                                in toJS g $ and $ evalGame (f pI >>= \x -> mapM isValid (Set.toList x)) g r'
    context "simpleOffers" $
      it "should not generate invalid actions" $ property $
        allValid simpleOffers
    context "possiblePurchases" $
      it "should not generate invalid actions" $ property $
        allValid possiblePurchases
    context "possibleDevelopmentCards" $
      it "should not generate invalid actions" $ property $
        allValid possibleDevelopmentCards
    context "possibleAccepts" $
      it "should generate accepts when an offer is present" $ property $ 
        \(Blind ng) -> let g = fromNormalGame ng
                           openTrades' = g^.openTrades
                           offer' = findS (isJust . toOffer) openTrades'
                           hasAcceptedAlready = Set.filter isAccept openTrades'
                           f x = let ask' = x^?! offer.asking
                                     asker' = x^?! offer.offeredBy
                                 in findKeyWhere (\p -> sufficient (p^.resources) ask' && (p^.playerIndex /= asker')) (g^.players)
                           hasEnough = fmap f offer'
                           oh = liftM2 (,) offer' (join hasEnough)
                           allExist = fmap (\(offer'', p') -> playersExist' [offer''^.offer.offeredBy, p'] g) oh
                       in Set.null hasAcceptedAlready && allExist == Just True ==> 
                         let stdGen = mkStdGen 0
                             accepts'' = evalGame possibleAccepts g stdGen
                         in toJS g $ not $ Set.null accepts''

    let game = head randomGames
    context "possibleTradeActions" $ do
      let offer' = TradeOffer mempty {ore = 1} mempty {wheat = 1} (toPlayerIndex 0)
      let accept' = accept offer' (toPlayerIndex 1)
      let complete' = fromJust $ complete $ accept'^?!action.trade
      let setAll = [ set (players . ix (toPlayerIndex 0) . resources) mempty { ore = 1 }
                   , set (players . ix (toPlayerIndex 1) . resources) mempty { wheat = 1 }
                   , set (players . ix (toPlayerIndex 2) . resources) mempty
                   , set openTrades (Set.singleton (Offer offer'))
                   , set currentPlayer (toPlayerIndex 0)
                   ]
      let offerGame = foldr (.) id setAll game
      let offerAndAcceptGame = set openTrades (Set.fromList [Offer offer', accept'^?!action.trade]) offerGame
      let getFromGame x g = evalGame x g (mkStdGen 0)
      let actionsFromGame = getFromGame (possibleTradeActions (toPlayerIndex 0))
      it "should generate an accept when offer is present" $
        actionsFromGame offerGame `shouldSatisfy` Set.member accept'
      it "should generate a complete when offer and acceptance are present" $
        actionsFromGame offerAndAcceptGame `shouldSatisfy` Set.member complete'

      let tradesFrom game' = getFromGame (use openTrades) (game' :: Game)
      let checkIfAny cImplies = anyS cImplies . tradesFrom
      let checkFor x game' = let acts' = actionsFromGame (game' :: Game)
                             in  anyS ((== Just True) . fmap x . preview (action.trade)) acts'
      it "should always generate an accept when offers are present" $ property $
        \(Blind ng) -> let g = fromNormalGame ng in checkIfAny (isJust . toOffer) g ==>
          toJS g $ checkFor isAccept g
      it "should always generate a reject when offers are present" $ property $
        \(Blind ng) -> let g = fromNormalGame ng in checkIfAny (isJust . toOffer) (g :: Game) ==>
          toJS g $ checkFor isReject g
      it "should never have an accept and reject by the same player" $ property $
        \(Blind ng) -> let g = fromNormalGame ng in checkIfAny (isJust . toOffer) g ==> 
          let trades' = tradesFrom g
              rejecters = Set.map (^?!rejecter) $ Set.filter isReject trades'
              accepters = Set.map (^?!accepter) $ Set.filter isAccept trades'
          in  toJS g $ Set.null (accepters `Set.intersection` rejecters)
      it "should always generate a complete when accepts are present" $ property $
        \(Blind ng) -> let g = fromNormalGame ng in checkIfAny isAccept (g :: Game) ==>
          toJS g $ checkFor isComplete g
      it "should never produce invalid actions" $ property $
        allValid possibleTradeActions
    context "getCard" $
      it "should add a card to a player's newCards field" $ property $
        \(Blind ng) -> let g = fromNormalGame ng
                           cardIs = views allCards head g
                           c' = Card cardIs
                           pI = view currentPlayer g
                           hasNewCard x = cardIs `elem` view (players . ix pI . newCards) x
                           hasCard x = c' `elem` view (players . ix pI . constructed) x
                           vA = view validActions g
                           endTurn = findS ((== EndTurn) . view action) vA
                           f x = let g' = fst $ runGame (update x) g (mkStdGen 0)
                                 in not (hasNewCard g') && hasCard g'
                           doesntKeep = maybe True f endTurn
                       in toJS g $ not (hasNewCard g) && doesntKeep
    context "makeDiscards" $
      it "should generate discards for players with >7 resources" $ do
        let setAll = [ set (players . ix p0 . resources) mempty { ore = 8 }
                     , set (players . ix p1 . resources) mempty { lumber = 9 }
                     , set (players . ix p2 . resources) mempty { brick = 3 }
                     ]
        let game' = foldr (.) id setAll game
        let discards = evalGame makeDiscards game' (mkStdGen 0)
        discards `shouldBe` Set.fromList [ mkDiscard (p0, mempty { ore = 4 })
                                         , mkDiscard (p1, mempty { lumber = 4 })]

sampleGameSpec :: Spec
sampleGameSpec = do
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

        let (g', r') = runGame (update pOffer') furtherAlong randRolled
        let others = views players (Map.filterWithKey (\k _ -> k /= pI)) g'
        let otherIs = Map.keysSet others
        let acceptancesAll = Set.map (accept tradeOffer') otherIs
        let validAcceptance g accept' = let thisPI = accept'^?!action.trade.accepter
                                            res = accept'^?!action.trade.offer.asking
                                        in  sufficient (g^.players.ix thisPI.resources) res
        let acceptances = Set.filter (validAcceptance g') acceptancesAll
        let rejections = Set.map (reject tradeOffer' Nothing) otherIs

        it "should add an offer to the open trades" $ do
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
        let (accepted, _) = runGame (update acceptance) g' randRolled

        let acceptance' = acceptance^?!action.trade
        let acceptedOffer = Offer (acceptance'^.offer)
        let aPI = acceptance^?!actor

        let beforeTradeP2 = furtherAlong^.players.ix aPI.resources
        let complete' = fromJust $ complete acceptance'

        it "should let another player accept" $ do
          view openTrades accepted `shouldBe` Set.fromList [acceptedOffer, acceptance']
          let pta = evalGame (possibleTradeActions pI) g' randRolled
          pta `shouldSatisfy` Set.member complete'
          let vA = Set.filter isTradeAction $ view validActions g'
          vA `shouldSatisfy` Set.member complete'

        let (completed, _) = runGame (update complete') accepted randRolled
        let afterTradeP1 = completed^.players.ix pI.resources
        let afterTradeP2 = completed^.players.ix aPI.resources

        it "should let the first player complete the trade" $ do
          view openTrades completed `shouldSatisfy` Set.null
          ore beforeTradeP1 `shouldSatisfy` (> (ore afterTradeP1))
          ore beforeTradeP2 `shouldSatisfy` (< (ore afterTradeP2))
          wheat beforeTradeP1 `shouldSatisfy` (< (wheat afterTradeP1))
          wheat beforeTradeP2 `shouldSatisfy` (> (wheat afterTradeP2))
      it "should transition to Special RobberAttack when a 7 is rolled" $ do
        let (robbed', _) = runGame updateForRoll rolled7 randRolled
        view phase robbed' `shouldBe` Special MovingRobber
      it "should transition to Special RobberAttack when a Knight is played" $ do
        let playCard' = mkPlayCard (toPlayerIndex 0) Knight
        let (g', _) = runGame (update playCard') withKnight randRolled
        view phase g' `shouldBe` Special MovingRobber
      it "should transition to Special FreeRoads when a RoadBuilding card is played" $ do
        let playCard' = mkPlayCard (toPlayerIndex 0) RoadBuilding
        let (g', _) = runGame (update playCard') withRoadBuilding randRolled
        view phase g' `shouldBe` Special (FreeRoads 2)
      it "should transition to Special Inventing when an Invention card is played" $ do
        let playCard' = mkPlayCard (toPlayerIndex 0) Invention
        let (g', _) = runGame (update playCard') withInvention randRolled
        view phase g' `shouldBe` Special Inventing
      it "should transition to Special Monopolizing when a Monopoly card is played" $ do
        let playCard' = mkPlayCard (toPlayerIndex 0) Monopoly
        let (g', _) = runGame (update playCard') withMonopoly randRolled
        view phase g' `shouldBe` Special Monopolizing

    context "in the Special phase" $ do
      context "RobberAttack" $ do
        let setAll = [ set (players . ix p0 . resources) mempty { ore = 8 }
                     , set (players . ix p1 . resources) mempty { wheat = 10 }
                     , set (players . ix p2 . resources) mempty { brick = 7 }
                     , set rolled $ Just 7
                     ]
        let toRobGame = (foldr (.) id setAll) rolledOnce
        let (robbedGame, _) = runGame updateForRoll toRobGame randRolled
        let disc1 = mkDiscard (p0, mempty { ore = 4 })
        let disc2 = mkDiscard (p1, mempty { wheat = 5 })
        it "should force any players with over 7 resources to discard half" $ do
          view phase robbedGame `shouldBe` Special RobberAttack
          let vA = view validActions robbedGame
          vA `shouldBe` Set.fromList [disc1, disc2]
        it "should transition into MovingRobber phase after discards" $ do
          let (postDiscard0, _) = runGame (update disc1) robbedGame randRolled
          let (postDiscard, _) = runGame (update disc2) postDiscard0 randRolled
          view phase postDiscard `shouldBe` Special MovingRobber
      context "MovingRobber" $ do
        describe "the player must move the robber" $ do
          context "when someone has >2 visible victory points" $ do
            specify "to a hex ringed by players with >2 visible victory points" $ property $
              \(Blind ng) -> let g = fromNormalGame ng
                                 scores' = evalGame scores (g :: Game) randRolled 
                             in any (>2) scores' ==>
                                  let (g', _) = runGame (forceRoll 7) g randRolled 
                                      psScores = zip (map toPlayerIndex [0..3]) scores'
                                      pColors = Map.map color $ view players g'
                                      high = map fst $ filter ((>2) . snd) psScores
                                      acceptableColors = mapMaybe (`Map.lookup` pColors) high
                                      vA = view validActions g'
                                      checkDest dest = all (`elem` acceptableColors) . map color $ evalGame (neighborBuildings dest) g' randRolled
                                      isValidRobberMove a = case a^.action of
                                                              SpecialAction (R (MoveRobber dest)) -> checkDest dest
                                                              _ -> False
                                  in  toJS g' $ allS isValidRobberMove vA
            specify "and should rob one of the players there" $ do
              pending
          specify "to an unoccupied hex, otherwise" $ property $
            \(Blind ng) -> 
               let g = fromNormalGame ng
                   scores' = evalGame scores g randRolled 
               in not (any (>2) scores') ==>
                       let (g', _) = runGame (forceRoll 7) g randRolled 
                           vA = view validActions g'
                           checkDest dest = null $ evalGame (neighborBuildings dest) g' randRolled
                           isValidRobberMove a = case a^.action of
                                                   SpecialAction (R (MoveRobber dest)) -> checkDest dest
                                                   _ -> False
                       in  toJS g' $ allS isValidRobberMove vA
      context "FreeRoads" $ do
        let playCard' = mkPlayCard (toPlayerIndex 0) RoadBuilding
        let (g', _) = runGame (update playCard') withRoadBuilding randRolled
        let pI = view currentPlayer g'
        let isFreeRoad x = isJust $ x ^? action.construct.onEdge

        let (built1, _) = runGame randomAct g' randRolled
        let (built2, _) = runGame randomAct built1 randRolled

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
          let (g', _) = runGame (update playCard') withInvention randRolled
          let inventions = Set.fromList $ map (invent p0) possibleInventions
          view validActions g' `shouldBe` inventions
          let lumberWheatInvention = PlayerAction p0 (SpecialAction (I (InventionOf mempty { lumber = 1, wheat = 1})))
          let (postLumberWheatInvention, _) = runGame (update lumberWheatInvention) g' randRolled

          let resPost = view (players . ix p0 . resources) postLumberWheatInvention
          let lumberPost = lumber resPost
          let wheatPost = wheat resPost
          lumberPost `shouldSatisfy` (== lumberPre + 1)
          wheatPost `shouldSatisfy` (== wheatPre + 1)
      context "Monopolizing" $
        it "should allow the current player to obtain all resources of a certain type" $ do
          let playCard' = mkPlayCard p0 Monopoly
          let (g', _) = runGame (update playCard') withMonopoly randRolled
          let monopolies = Set.fromList $ map (PlayerAction p0 . SpecialAction) possibleMonopolies
          view validActions g' `shouldBe` monopolies

          let lumberMonopoly = PlayerAction p0 (SpecialAction (M (MonopolyOn Lumber)))
          let (postMonopoly, _) = runGame (update lumberMonopoly) g' randRolled

          let pRes = Map.elems . Map.map (view resources) $ postMonopoly^.players
          let totalLumber = sum $ map lumber pRes
          pRes `shouldSatisfy` all (\r -> lumber r == 0 || lumber r == totalLumber)
    context "in End phase" $
      it "has no more validActions" $ property $
        \(Blind g) -> view phase (g :: Game) == End ==>
          toJS g $ Set.null $ view validActions g
  describe "updateBonuses" $ do
    context "when comparing roads" $ do
      it "should grant the LongestRoad when the first player has a road of length 5" $ do
        pending
      it "should transfer the LongestRoad when a player's road has been surpassed" $ do
        pending
    context "when comparing armies" $ do
      it "should grant the LargestArmy when the first player has an army of size 3" $ do
        pending
      it "should transfer the LargestArmy when a player's army has been surpassed" $ do
        pending
