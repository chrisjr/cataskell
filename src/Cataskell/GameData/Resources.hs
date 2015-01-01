{-# LANGUAGE DeriveGeneric #-}

module Cataskell.GameData.Resources where

import Cataskell.GameData.Basics
import Data.Monoid
import Data.Aeson
import GHC.Generics (Generic)

data ResourceCount = ResourceCount 
  { lumber :: Int
  , wool :: Int
  , wheat :: Int
  , brick :: Int
  , ore :: Int
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON ResourceCount
instance ToJSON ResourceCount

instance Monoid ResourceCount where
  mempty = ResourceCount 0 0 0 0 0
  mappend r1 r2 = ResourceCount
    { lumber = lumber r1 + lumber r2
    , wool = wool r1 + wool r2
    , wheat = wheat r1 + wheat r2
    , brick = brick r1 + brick r2
    , ore = ore r1 + ore r2
    }

totalResources :: ResourceCount -> Int
totalResources r = lumber r + wool r + wheat r + brick r + ore r

mkNeg :: ResourceCount -> ResourceCount
mkNeg r = ResourceCount
  { lumber = -lumber r
  , wool = -wool r
  , wheat = -wheat r
  , brick = -brick r
  , ore = -ore r
  }

sufficient :: ResourceCount -> ResourceCount -> Bool
sufficient r c = and [lumber', wool', wheat', brick', ore']
  where lumber' = lumber r >= lumber c
        wool' = wool r >= wool c
        wheat' = wheat r >= wheat c
        brick' = brick r >= brick c
        ore' = ore r >= ore c

cost :: PotentialItem -> ResourceCount
cost c = case c of
  Potential (HabitationToBe Settlement) -> mempty { lumber = 1, brick = 1, wool = 1, wheat = 1 }
  Potential (HabitationToBe City) -> mempty { wheat = 2, ore = 3 }
  Potential (RoadToBe Road) ->  mempty { lumber = 1, brick = 1 }
  DevCard -> mempty { wool = 1, wheat = 1, ore = 1 }

payFor :: ResourceCount -> PotentialItem -> Maybe ResourceCount
payFor r c
  = if sufficient r expense then Just (r <> mkNeg expense) else Nothing
  where expense = cost c

nonNegative :: ResourceCount -> Bool
nonNegative = (flip sufficient) mempty

resourceFromTerrain :: Terrain -> ResourceCount
resourceFromTerrain t
  = case t of
      Forest -> mempty { lumber = 1 }
      Pasture -> mempty { wool = 1 }
      Field -> mempty { wheat = 1 }
      Hill  -> mempty { brick = 1 }
      Mountain -> mempty { ore = 1 }
      Desert -> mempty