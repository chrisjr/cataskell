module Cataskell.GameSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Cataskell.Game
import Cataskell.GameData.Actions
import Cataskell.GameData.Basics
import Cataskell.GameData.Board
import Cataskell.GameData.Player
import Cataskell.GameData.Resources
import Control.Monad.Random
import Control.Monad.Identity
import Control.Monad.State
import Data.List
import Data.Maybe (fromJust, isJust)
import Data.Monoid (mempty)
import Control.Lens hiding (elements)
import Cataskell.UtilSpec() -- for Arbitrary StdGen instance

newtype InitialGame = InitialGame Game
  deriving (Eq, Ord, Show, Read)

toInitialGame :: Game -> InitialGame
toInitialGame x | x ^. phase == Initial = InitialGame x
                | otherwise = error "Game state is not initial"

fromInitialGame :: InitialGame -> Game
fromInitialGame (InitialGame x) = x

instance Arbitrary Game where
  arbitrary = do
    initial <- arbitrary
    return $ fromInitialGame initial

instance Arbitrary InitialGame where
  arbitrary = do
    stdGen <- (arbitrary :: Gen StdGen)
    names <- elements [["1", "2", "3"], ["1", "2", "3", "4"]]
    return $ toInitialGame $ evalRand (newGame names) stdGen

-- newtype GameStateStd = GameStateStd (StateT Game (RandT StdGen Identity) ())

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "A new game" $ do
    it "should start in the Initial phase" $ property $
      \g -> (view phase $ fromInitialGame (g :: InitialGame)) == Initial
  describe "Game invariants" $ do
    it "must have either 3 or 4 players" $ property $
      \game -> let l = length $ view players (game :: Game)
               in l == 3 || l == 4
    it "should allow for the retrieval of a specific player" $ property $
      \game stdGen -> let i' = evalRand (evalStateT (findPlayerByColor Blue) (game :: Game)) (stdGen :: StdGen)
                          i = fromPlayerIndex i'
                      in i >= 0 && i < 4
    it "has a list of valid next actions, except at the end" $ property $
      \game -> let n = (game :: Game) ^.validActions.to length
               in (n > 0) || (view phase game == End)
    it "has at most one player with >= 10 victory points" $ property $
      \game -> let scores = map (view score) $ game ^. players
                   highestCount = last . map (\xs -> (head xs, length xs)) . group $ sort scores
               in  (fst highestCount >= 10) == (snd highestCount == 1)

  describe "An example game" $ do

    let (initialGame, r') = runRand (newGame ["1", "2", "3", "4"]) (mkStdGen 0)

    let gs = iterate (\(x, r) -> runRand (execStateT randomAct x) r) (initialGame, r')
    let (normalGame, _) = gs !! 16

    context "in Initial phase" $ do
      it "should initially allow a settlement built anywhere" $ do
        let valids = view validActions initialGame
        length valids `shouldBe` 54
      context "when cycling through players forward then back" $ do
        let playerIs = [0, 0, 1, 1, 2, 2, 3, 3, 3, 3, 2, 2, 1, 1, 0, 0]

        forM_ (zip3 [0..] playerIs (map fst gs)) $ \(i, c, g) ->
          it ("should permit placement for player " ++ (show c)) $ do
            view currentPlayer g `shouldBe` toPlayerIndex c
            views validActions length g `shouldSatisfy` (<= (54 - ((i `div` 2) * 3))) 
      it "should transition to Normal phase when placements are complete" $ do
        view phase normalGame `shouldBe` Normal
        views validActions length normalGame `shouldBe` 1

    -- produce some variants that will be used later

    let (rolledOnce, randRolled) = gs !! 17

    let withRoadBuilding = (players . ix 0 . constructed) <>~ [Card RoadBuilding] $ rolledOnce
    let rolled7 = rolled .~ Just 7 $ rolledOnce

    context "in Normal phase" $ do
      it "should start each turn with a roll" $ do
        let vA = view validActions normalGame
        let p1 = views players head normalGame
        vA `shouldBe` [rollFor (p1^.playerIndex)]
        view turnAdvanceBy normalGame `shouldBe` 1

      it "should distribute resources once a roll happens" $ do
        let totalAsOf g = sum . map (views resources totalResources) $ view players g
        let (starting, _)  = gs !! 16
        totalAsOf rolledOnce `shouldSatisfy` (> (totalAsOf starting))

      let pI = view currentPlayer rolledOnce
      let pOffer = mkOffer pI (mempty { ore = 1} ) (mempty { wheat = 1 } )
      let tradeOffer' = fromJust $ pOffer ^? action.trade.offer

      it "should allow for a trade offer" $ do
        let vA = view validActions rolledOnce
        vA `shouldSatisfy` (any (actionLike pOffer))

      context "when trading" $ do
        it "should add it to the open trades" $ do
          let (g', r') = runGame (update pOffer) rolledOnce randRolled
          view openTrades g' `shouldBe` [Offer tradeOffer']
      it "should allow for building, according to resources" $ do
        let enoughForSettlement p = sufficient (p^.resources) (cost $ unbuilt settlement)
        let unbuiltLeft p = any (== (unbuilt settlement)) (p^.constructed)
        let spaceOnBoard p g = not . null $ validSettlementsFor (color p) (g^.board)
        let canBuildSettlement p = enoughForSettlement p && unbuiltLeft p
        let isCurrentPlayer p g = p^.playerIndex == view currentPlayer g
        let isBuildSettlement x = isSettlement `fmap` (x ^? action.item) == Just True
        let suitable (p, g) = canBuildSettlement p && isCurrentPlayer p g && spaceOnBoard p g
        let findSuitable g = findIndex suitable $ zip (view players g) (repeat g) 
        let maybeGame = find (\(g, _) -> isJust $ findSuitable g) $ take 200 $ drop 16 gs
        if isJust maybeGame
        then do
          let g' = fst $ fromJust maybeGame
          let vA = view validActions g'
          -- let suitablePI = toPlayerIndex findSuitable g' 
          vA `shouldSatisfy` (any isBuildSettlement)
        else fail "couldn't find a player able to build a settlement after 200 actions"
      it "should transition to Special RobberAttack when a 7 is rolled" $ do
        let (robbed', _) = runGame updateForRoll rolled7 randRolled
        view phase robbed' `shouldBe` Special MovingRobber
      it "should transition to Special FreeRoads when a RoadBuilding card is played" $ do
        let playCard' = mkPlayCard (toPlayerIndex 0) RoadBuilding
        let (g', _) = runGame (update playCard') withRoadBuilding randRolled
        view phase g' `shouldBe` Special (FreeRoads 2)
      it "should transition to Special Inventing when an Invention card is played" $ do
        pending
      it "should transition to Special Monopolizing when a Monopoly card is played" $ do
        pending

    context "in the Special phase" $ do
      context "RobberAttack" $ do
        it "should force any players with over 7 resources to discard half" $ do
          pending
        it "should transition into MovingRobber phase after discards" $ do
          pending
      context "MovingRobber" $ do
        describe "the player must move the robber" $ do
          specify "to a hex neighbored solely by players with >2 visible victory points" $ do
            pending
          specify "to an unoccupied hex, otherwise" $ do
            pending
      context "FreeRoads" $ do
        let playCard' = mkPlayCard (toPlayerIndex 0) RoadBuilding
        let (g', _) = runGame (update playCard') withRoadBuilding randRolled
        let pI = view currentPlayer g'
        let isFreeRoad x = isJust $ x ^? action.construct.onEdge

        let (built1, _) = runGame (randomAct) g' randRolled
        let (built2, _) = runGame (randomAct) built1 randRolled

        it "should allow the current player to build two roads" $ do
          view validActions g' `shouldSatisfy` (all isFreeRoad)
          view validActions built1 `shouldSatisfy` (all isFreeRoad)
        it "should return to normal after two roads are built" $ do
          let vA = view validActions built2
          vA `shouldSatisfy` (not . any isFreeRoad)
          vA `shouldSatisfy` (elem (mkEndTurn pI))
          view phase built2 `shouldBe` Normal
      context "Inventing" $ do
        it "should allow the current player to obtain two resources of their choice" $ do
          pending
      context "Monopolizing" $ do
        it "should allow the current player to obtain all resources of a certain type" $ do
          pending

    context "in End phase" $ do
      it "has no more validActions" $ do
        pending
