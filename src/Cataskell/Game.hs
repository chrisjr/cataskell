module Cataskell.Game where

import Control.Monad.Random
import Cataskell.GameData

newGame :: (RandomGen g) => Rand g GameState
newGame = undefined