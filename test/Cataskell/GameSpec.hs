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
import Data.Maybe (fromJust)
import Control.Lens ((^.), (^..), view, views, to, use, uses)
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
      \game stdGen -> let i = evalRand (evalStateT (findPlayerByColor Blue) (game :: Game)) (stdGen :: StdGen)
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
            view currentPlayer g `shouldBe` c
            views validActions length g `shouldSatisfy` (<= (54 - ((i `div` 2) * 3))) 
      it "should transition to Normal phase when placements are complete" $ do
        view phase normalGame `shouldBe` Normal
        views validActions length normalGame `shouldBe` 1

    context "in Normal phase" $ do
      it "should start each turn with a roll" $ do
        let vA = view validActions normalGame
        let p1 = views players head normalGame
        vA `shouldBe` [rollFor p1]
      it "should distribute resources once a roll happens" $ do
        let totalAsOf g = sum . map (views resources totalResources) $ view players g
        let (starting, _)  = gs !! 16
        let (rolled, _) = gs !! 17
        totalAsOf rolled `shouldSatisfy` (> (totalAsOf starting))
      it "should allow for trade offers" $ do
        pending
      it "should allow for building, according to resources" $ do
        pending
      it "should transition to RobberAttack phase when a 7 is rolled" $ do
        pending

    context "in RobberAttack phase" $ do
      it "should force any players with over 7 resources to discard half" $ do
        pending
      it "should transition into MovingRobber phase after discards" $ do
        pending

    context "in MovingRobber phase" $ do
      describe "the player must move the robber" $ do
        specify "to a hex neighbored solely by players with >2 visible victory points" $ do
          pending
        specify "to an unoccupied hex, otherwise" $ do
          pending

    context "in End phase" $ do
      it "has no more validActions" $ do
        pending
