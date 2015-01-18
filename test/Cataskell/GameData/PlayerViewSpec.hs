{-# LANGUAGE RankNTypes #-}
module Cataskell.GameData.PlayerViewSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Applicative
import Control.Lens hiding (elements)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Cataskell.Game
import Cataskell.GameData.Actions
import Cataskell.GameData.Player (validPlayer, playerIndex, toPlayerIndex)
import Cataskell.GameData.PlayerView
import Cataskell.Util
import Cataskell.GameSpec()

instance Arbitrary PlayerView where
  arbitrary = do
    game <- arbitrary :: Gen Game
    pI <- elements (Map.keys $ game^.players)
    return $ viewFor pI game


-- | Confirm that all players see the same thing
checkView :: (Eq a) => Getter Game a -> (PlayerView -> a) -> Game -> Bool
checkView l r g
  = let pIs = Map.keys (g^.players)
    in all (\pI -> r (viewFor pI g) == g^.l) pIs

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "A PlayerView" $ do
    it "should have a phase" $ property $
      checkView phase _pvphase
    it "should have a board" $ property $
      checkView board _pvboard
    it "should have a currentPlayer" $ property $
      checkView currentPlayer _pvcurrentPlayer
    it "should have a valid player" $ property $
      validPlayer . _pvself
    it "should have a map of other players that doesn't include that player" $ property $
      \pv -> let me = _pvself pv ^. playerIndex
                 others' = Map.keysSet $ _pvothers pv
             in not (Set.null others') && not (me `Set.member` others')
    it "should have the current dice roll" $ property $
      checkView rolled _pvrolled
    it "should have a set of open trades" $ property $
      checkView openTrades _pvopenTrades
    it "should have a list of valid actions specific to current player" $ property $
      \pv -> allS (== _pvself pv ^. playerIndex) $ Set.map (^.actor) (_pvvalidActions pv)
    it "should have a winner if one exists" $ property $
      checkView winner _pvwinner
