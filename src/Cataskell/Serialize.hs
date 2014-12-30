{-# OPTIONS_GHC -fno-warn-orphans #-}
module Cataskell.Serialize where

import Cataskell.GameData.Basics
import Cataskell.GameData.Location
import Data.Aeson

-- location instances

instance ToJSON VertexPosition
instance FromJSON VertexPosition

instance ToJSON Point
instance FromJSON Point

instance ToJSON UndirectedEdge
instance FromJSON UndirectedEdge

-- gameData instances

instance ToJSON Terrain
instance FromJSON Terrain

instance ToJSON Color
instance FromJSON Color

instance ToJSON DevelopmentCard
instance FromJSON DevelopmentCard