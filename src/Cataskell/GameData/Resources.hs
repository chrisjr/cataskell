{-# LANGUAGE DeriveGeneric #-}

module Cataskell.GameData.Resources where

import Cataskell.GameData.Basics
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)

data ResourceCount = ResourceCount 
  { lumber :: Int
  , wool :: Int
  , wheat :: Int
  , brick :: Int
  , ore :: Int
  } deriving (Eq, Ord, Show, Read,Generic)

instance Monoid ResourceCount where
  mempty = ResourceCount 0 0 0 0 0
  mappend r1 r2 = ResourceCount
    { lumber = lumber r1 + lumber r2
    , wool = wool r1 + wool r2
    , wheat = wheat r1 + wheat r2
    , brick = brick r1 + brick r2
    , ore = ore r1 + ore r2
    }

data HarborDiscount = HarborDiscount { consumes :: ResourceCount }
  deriving (Eq, Ord, Show, Read)

totalResources :: ResourceCount -> Int
totalResources r = lumber r + wool r + wheat r + brick r + ore r

mulResources :: ResourceCount -> Int -> ResourceCount
mulResources r i = ResourceCount
  { lumber = i * lumber r
  , wool = i * wool r
  , wheat = i * wheat r
  , brick = i * brick r
  , ore = i * ore r
  }

mkNeg :: ResourceCount -> ResourceCount
mkNeg r = mulResources r (-1)

sufficient :: ResourceCount -> ResourceCount -> Bool
sufficient r c = and [lumber', wool', wheat', brick', ore']
  where lumber' = lumber r >= lumber c
        wool' = wool r >= wool c
        wheat' = wheat r >= wheat c
        brick' = brick r >= brick c
        ore' = ore r >= ore c

cost :: Item -> ResourceCount
cost c = byItemType itemType'
  where itemType' = case c of
          Building x -> case x of 
            Edifice (OnPoint _ _ h) -> H h
            Roadway (OnEdge _ _) -> Road
          Card _ -> DevelopmentCard
          Potential x -> x
        byItemType x = case x of
          H Settlement -> mempty { lumber = 1, brick = 1, wool = 1, wheat = 1 }
          H City -> mempty { wheat = 2, ore = 3 }
          Road ->  mempty { lumber = 1, brick = 1 }
          DevelopmentCard -> mempty { wool = 1, wheat = 1, ore = 1 }

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

filteredResCount :: ResourceType -> ResourceCount -> ResourceCount
filteredResCount resType res
  = case resType of
      Lumber -> mempty { lumber = lumber res}
      Wool -> mempty { wool = wool res }
      Wheat -> mempty { wheat = wheat res }
      Brick -> mempty { brick = brick res }
      Ore -> mempty { ore = ore res }      

nResOf :: Int -> ResourceType -> ResourceCount
nResOf i resType
  = case resType of
      Lumber -> mempty { lumber = i}
      Wool -> mempty { wool = i }
      Wheat -> mempty { wheat = i }
      Brick -> mempty { brick = i }
      Ore -> mempty { ore = i }      

genericHarborDiscount :: Int -> Set HarborDiscount
genericHarborDiscount i
  = Set.fromList $ map (mkDiscount i) [Lumber, Wool, Wheat, Brick, Ore]

mkDiscount :: Int -> ResourceType -> HarborDiscount
mkDiscount i resType = HarborDiscount (nResOf i resType)

applyDiscount :: ResourceCount -> HarborDiscount -> (Int, ResourceCount)
applyDiscount playerRes harbor = go 0 playerRes
  where res = consumes harbor 
        go n remainingRes = if not $ sufficient remainingRes res
                               then (n, remainingRes) 
                               else go (n+1) (remainingRes <> mkNeg res)

harborDiscount :: Harbor -> Set HarborDiscount
harborDiscount harbor'
  = case harbor' of
      Harbor Hill -> Set.singleton (mkDiscount 2 Brick)
      Harbor Forest -> Set.singleton (mkDiscount 2 Lumber)
      Harbor Pasture -> Set.singleton (mkDiscount 2 Wool)
      Harbor Field -> Set.singleton (mkDiscount 2 Wheat)
      Harbor Mountain -> Set.singleton (mkDiscount 2 Ore)
      Harbor Desert -> Set.empty
      ThreeToOne -> genericHarborDiscount 3
