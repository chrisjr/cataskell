module Cataskell.GameSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Cataskell.Game
import Cataskell.GameData.Actions
import Cataskell.GameData.Basics
import Cataskell.GameData.Board
import Cataskell.GameData.Player
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
  describe "Any Game" $ do
    it "must have either 3 or 4 players" $ property $
      \game -> let l = length $ view players (game :: Game)
               in l == 3 || l == 4
    it "should allow for the retrieval of a specific player" $ property $
      \game stdGen -> let i = evalRand (evalStateT (findPlayer Blue) (game :: Game)) (stdGen :: StdGen)
                      in i >= 0 && i < 4
    it "has a list of valid next actions, except at the end" $ property $
      \game -> let n = (game :: Game) ^.validActions.to length
               in (n > 0) || (view phase game == End)
    it "has at most one player with >= 10 victory points" $ property $
      \game -> let scores = map (view score) $ game ^. players
                   highestCount = last . map (\xs -> (head xs, length xs)) . group $ sort scores
               in  (fst highestCount >= 10) == (snd highestCount == 1)

  describe "The update function" $ do

    let (initialGame, r') = runRand (newGame ["1", "2", "3", "4"]) (mkStdGen 0)
    context "in Initial phase" $ do
      it "should initially allow a settlement built anywhere" $ do
        let valids = view validActions initialGame
        length valids `shouldBe` 54

      let gs = iterate (\x -> runGame progress x r') initialGame
      context "when cycling through players forward then back" $ do
        let playerIs = [0, 1, 2, 3, 3, 2, 1, 0]

        forM_ (zip playerIs gs) $ \(i, g) ->
          it ("should permit placement for player " ++ (show i)) $ do
            view currentPlayer g `shouldBe` i
            views validActions length g `shouldBe` 54

      it "should transition to Normal phase when placements are complete" $ do
        let g = gs !! 8
        view phase g `shouldBe` Normal
        views validActions length g `shouldBe` 1

    let normalGame = runGame initialToNormal initialGame (mkStdGen 0)
    context "in Normal phase" $ do
      it "should start each turn with a roll" $ do
        let vA = evalState (use validActions) normalGame
        let p1 = evalState (uses players head) normalGame
        vA `shouldBe` [rollFor p1]
      it "should distribute resources once a roll happens" $ do
        pending
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
