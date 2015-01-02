module Cataskell.GameSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Cataskell.Game
import Cataskell.GameData.Basics
import Cataskell.GameData.Board
import Cataskell.GameData.Player
import Control.Monad.Random
import Data.Maybe (fromJust)
import Cataskell.UtilSpec() -- for Arbitrary StdGen instance

newtype InitialGameState = InitialGame GameState
  deriving (Eq, Ord, Show, Read)

toInitial :: GameState -> InitialGameState
toInitial x | phase x == Initial = InitialGame x
           | otherwise = error "Game state is not initial"

fromInitial :: InitialGameState -> GameState
fromInitial (InitialGame x) = x

instance Arbitrary GameState where
  arbitrary = do
    initial <- arbitrary
    return $ fromInitial initial

instance Arbitrary InitialGameState where
  arbitrary = do
    seed <- arbitrary
    names <- elements [["1", "2", "3"], ["1", "2", "3", "4"]]
    return $ toInitial $ evalRand (newGame names) (mkStdGen seed)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "A new game" $ do
    it "should start in the Initial phase" $ property $
      \g -> (phase $ fromInitial (g :: InitialGameState)) == Initial
  describe "Any GameState" $ do
    it "must have either 3 or 4 players" $ property $
      \game -> let l = length $ players (game :: GameState)
               in l == 3 || l == 4
    it "should allow for the retrieval of a specific player" $ property $
      \game -> let p = getPlayer Blue (game :: GameState)
               in validPlayer $ fromJust p
    it "has a list of valid next actions, except at the end" $ property $
      \game -> let n = length $ validActions (game :: GameState)
               in (n > 0) || (phase game == End) 
