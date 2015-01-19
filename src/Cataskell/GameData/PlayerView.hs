module Cataskell.GameData.PlayerView where

import Control.Lens
import Control.Exception (assert)
import Cataskell.Game
import Cataskell.GameData.Actions
import Cataskell.GameData.Board
import Cataskell.GameData.Player

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- | A player's view of the game state
data PlayerView = PlayerView
  { _pvphase :: Phase
  , _pvboard :: Board
  , _pvcurrentPlayer :: PlayerIndex
  , _pvself :: Player
  , _pvothers :: Map PlayerIndex Player
  , _pvrolled :: Maybe Int
  , _pvopenTrades :: Set TradeAction
  , _pvvalidActions :: Set PlayerAction -- only for this player
  , _pvwinner :: Maybe PlayerIndex
  }
  deriving (Eq, Show)

viewFor :: PlayerIndex -> Game -> PlayerView
viewFor pI g
  = assert (pI `Map.member` (g^.players)) 
    PlayerView
      { _pvphase = g^.phase
      , _pvboard = g^.board
      , _pvcurrentPlayer = g^.currentPlayer
      , _pvself = (g^.players) Map.! pI
      , _pvothers = Map.filterWithKey (\k _ -> k /= pI) $ g^.players
      , _pvrolled = g^.rolled
      , _pvopenTrades = g^.openTrades
      , _pvvalidActions = Set.map (^.action) . Set.filter ((== pI) . view actor) $ g^.validActions
      , _pvwinner = g^.winner
      }
