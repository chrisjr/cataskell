{-# LANGUAGE DeriveGeneric #-}
module Cataskell.Game where

import Control.Monad.Random
import Cataskell.GameData.Board
import GHC.Generics (Generic)

data Phase = NotStarted | Initial | Normal | End
  deriving (Eq, Show, Read,Ord, Generic)

data GameState = GameState
  { board :: Board
  , phase :: Phase
  } deriving (Eq, Show, Read,Generic)

newGame :: (RandomGen g) => Rand g GameState
newGame = undefined
