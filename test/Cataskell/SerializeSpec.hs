{-# LANGUAGE ScopedTypeVariables #-}

module Cataskell.SerializeSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Data.Aeson
import Data.Map.Strict (Map)
import Cataskell.Game
import Cataskell.GameData.Actions
import Cataskell.GameData.Basics
import Cataskell.GameData.Board
import Cataskell.GameData.Location
import Cataskell.GameData.Player
import Cataskell.GameData.PlayerView
import Cataskell.GameData.Resources

import Cataskell.GameSpec()
import Cataskell.GameData.ActionsSpec()
import Cataskell.GameData.BasicsSpec()
import Cataskell.GameData.BoardSpec()
import Cataskell.GameData.LocationSpec()
import Cataskell.GameData.PlayerSpec()
import Cataskell.GameData.PlayerViewSpec()
import Cataskell.GameData.ResourcesSpec()

import Cataskell.Serialize

roundTrip :: forall a.  (ToJSON a
                       , FromJSON a
                       , Arbitrary a
                       , Eq a
                       , Show a) => String -> a -> Spec
roundTrip typeName _ =
  describe typeName $
    it "should be converted to and from JSON" $ property $
      \(x :: a) -> fromJSON (toJSON x) == Data.Aeson.Success x

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  -- roundTrip "VertexPosition" (undefined :: VertexPosition) -- included in Point
  roundTrip "Point" (undefined :: Point)
  roundTrip "CentralPoint" (undefined :: CentralPoint)
  roundTrip "UndirectedEdge" (undefined :: UndirectedEdge)
  roundTrip "ResourceType" (undefined :: ResourceType)
  roundTrip "ResourceCount" (undefined :: ResourceCount)
  -- roundTrip "Terrain" (undefined :: Terrain) -- included in HexCenter
  roundTrip "Color" (undefined :: Color)
  roundTrip "DevelopmentCard" (undefined :: DevelopmentCard)
  roundTrip "Inhabited" (undefined :: Inhabited)
  -- roundTrip "ItemType" (undefined :: ItemType) -- included in potential Items
  roundTrip "OnPoint" (undefined :: OnPoint)
  roundTrip "OnEdge" (undefined :: OnEdge)
  roundTrip "Construct" (undefined :: Construct)
  roundTrip "Item" (undefined :: Item)
  roundTrip "Harbor" (undefined :: Harbor)
  roundTrip "HexCenter" (undefined :: HexCenter)
  roundTrip "HarborMap" (undefined :: HarborMap)
  roundTrip "PlayerMap" (undefined :: Map PlayerIndex Player)
  roundTrip "HexMap" (undefined :: Map CentralPoint HexCenter)
  roundTrip "BuildingMap" (undefined :: BuildingMap)
  roundTrip "RoadMap" (undefined :: RoadMap)
  roundTrip "Board" (undefined :: Board)
  roundTrip "PlayerIndex" (undefined :: PlayerIndex)
  roundTrip "Bonus" (undefined :: Bonus)
  roundTrip "Player" (undefined :: Player)
  roundTrip "Monopoly" (undefined :: Monopoly)
  roundTrip "Invention" (undefined :: Invention)
  roundTrip "MoveRobber" (undefined :: MoveRobber)
  roundTrip "SpecialAction" (undefined :: SpecialAction)
  roundTrip "TradeOffer" (undefined :: TradeOffer)
  roundTrip "TradeAction" (undefined :: TradeAction)
  roundTrip "DiscardAction" (undefined :: DiscardAction)
  roundTrip "PlayerAction" (undefined :: PlayerAction)
  roundTrip "GameAction" (undefined :: GameAction)
  roundTrip "SpecialPhase" (undefined :: SpecialPhase)
  roundTrip "Phase" (undefined :: Phase)
  roundTrip "Game" (undefined :: Game)
