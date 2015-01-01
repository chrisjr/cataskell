module Cataskell.GameSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Cataskell.Game
import Cataskell.GameData.Basics
import Cataskell.GameData.Board
import Control.Monad.Random
import Cataskell.UtilSpec() -- for Arbitrary StdGen instance

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "A new game" $ do
    let mkGame g = (evalRand newGame (g :: StdGen))
    it "should start in the Initial phase" $ property $
      \g -> phase (mkGame g) == Initial
