-- | Generate a board with connectivity information.

module Cataskell.BoardGraph where

import Data.Graph.Inductive
import Data.Graph.Inductive.PatriciaTree()
import Data.Maybe (listToMaybe, fromJust)
import Control.Applicative ((<$>))
import Control.Monad
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Cataskell.Util
import Cataskell.GameData.Location ( hexCenterPoints
                                   , hexNeighborhoods
                                   , mkHexPointsAndEdges
                                   , fromCenter
                                   , CentralPoint(..)
                                   , Point(..)
                                   , UndirectedEdge(..)
                                   , EdgeType(..)
                                   , edgeType
                                   , edgePoints
                                   , dupleToEdge)
-- import qualified Cataskell.GameData as GD

type BoardGraph = Gr Point UndirectedEdge
type BoardNodeContext = Context Point UndirectedEdge

-- | Find adjacents and the node itself
getContextByLabel :: (Eq a) => a -> Gr a b -> Maybe (Context a b)
getContextByLabel point gr = listToMaybe $ gsel (\x -> lab' x == point) gr

-- | Get a node based on its point definition.
getNodeMaybe :: (Eq a) => a -> Gr a b -> Maybe Node 
getNodeMaybe point gr = node' <$> getContextByLabel point gr

-- | Get an edge based on its label.
getEdgeMaybe :: (Eq b) => b -> Gr a b -> Maybe (LEdge b)
getEdgeMaybe edge gr = listToMaybe . filter (\(_, _, x) -> x == edge) $ labEdges gr

-- | Insert a node iff it doesn't yet exist in the graph
insNodeOnce :: (Eq a) => Gr a b -> a -> Gr a b
insNodeOnce gr p 
  = case getNodeMaybe p gr of 
      Just _  -> gr
      Nothing -> let n = snd (nodeRange gr) + 1
                     updatedGr = insNode (n, p) gr
                 in  updatedGr

-- | Insert multiple nodes exactly once.
insNodesOnce :: (Eq a) => [a] -> Gr a b -> Gr a b
insNodesOnce ps gr
  = foldl insNodeOnce gr ps

-- | Insert an edge iff it doesn't yet exist in the graph.
insEdgeOnce :: BoardGraph -> UndirectedEdge -> BoardGraph
insEdgeOnce gr uEdge 
  = case getEdgeMaybe uEdge gr of 
      Just _  -> gr
      Nothing -> insEdge (x, y, uEdge) gr
    where x = fromJust $ getNodeMaybe (point1 uEdge) gr
          y = fromJust $ getNodeMaybe (point2 uEdge) gr

-- | Insert multiple edges iff they don't yet exist in the graph.
insEdgesOnce :: [UndirectedEdge] -> BoardGraph -> BoardGraph
insEdgesOnce uEdges gr
 = foldl insEdgeOnce gr uEdges

-- | Add a hex to the graph at specified coordinate, reusing vertices if possible.
addHex :: BoardGraph -> CentralPoint -> BoardGraph
addHex gr centerPoint
  = let (points, uEdges) = mkHexPointsAndEdges centerPoint
        nodeGr = insNodesOnce points gr
        gr' = insEdgesOnce uEdges nodeGr
    in  undir gr'

connectCenters :: BoardGraph -> BoardGraph
connectCenters = insEdgesOnce centerEdges
  where centerEdges = concatMap (\x -> map (curry dupleToEdge (fromCenter x)) (getNs x)) hexCenterPoints
        getNs = map fromCenter . (Map.!) hexNeighborhoods

-- | The Catan board as a graph. Occupancy data is stored in a map (see Cataskell.GameData.Board)
boardGraph :: BoardGraph
boardGraph = undir $ connectCenters gr
             where gr = foldl addHex empty hexCenterPoints

-- | Count the nodes that satisfy a predicate.
countNodes :: (Point -> Bool) -> BoardGraph -> Int
countNodes p gr = length $ filter p $ map snd $ labNodes gr

neighborPoints :: BoardGraph -> Point -> Set Point
neighborPoints gr p
  = let thisNode = fromJust $ getNodeMaybe p gr
        nn = neighbors gr thisNode
    in  Set.fromList $ map snd . filter (\(n, _) -> n `elem` nn) $ labNodes gr

connections :: (EdgeType -> Bool) -> BoardGraph
connections etP = elfilter (etP . edgeType) boardGraph

centerConnections :: BoardGraph
centerConnections = connections (== BetweenCenters)

resourceConnections :: BoardGraph
resourceConnections = connections (== ToCenter)

roadConnections :: BoardGraph
roadConnections = connections (== Between)

allEdges :: Set UndirectedEdge
allEdges = Set.fromList $ map (\(_, _, x) -> x) $ labEdges roadConnections

roadGraph :: Set UndirectedEdge -> Set Point -> Gr UndirectedEdge (UndirectedEdge, UndirectedEdge)
roadGraph s enemyPs
  = let sharedPoint e1 e2 = edgePoints e1 `Set.intersection` edgePoints e2
        valid e1 e2 = let p = sharedPoint e1 e2
                      in Set.size p == 1 && fmap (`Set.notMember` enemyPs) (firstMaybe p) == Just True
        s' = Set.toList s
        g = insNodesOnce s' empty
        uEdges = [(s1,s2) | s1 <- s', s2 <- s', valid s1 s2, s1 < s2]
        insEdge' gr (s1, s2) = fromJust $ f s1' s2'
                                 where f = liftM2 (\x y -> insEdge (x,y,(s1,s2)) gr)
                                       s1' = getNodeMaybe s1 gr
                                       s2' = getNodeMaybe s2 gr
    in foldl insEdge' g uEdges

boardPrint :: BoardGraph -> IO ()
boardPrint = prettyPrint
