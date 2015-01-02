-- | Generate a board with connectivity information.

module Cataskell.BoardGraph where

import Data.Graph.Inductive
import Data.Graph.Inductive.PatriciaTree()
import Data.Maybe (listToMaybe, fromJust)
import Control.Applicative ((<$>))
import qualified Data.Map.Strict as Map
import Cataskell.GameData.Location ( hexCenterPoints
                                   , hexNeighborhoods
                                   , mkHexPointsAndEdges
                                   , toCenter
                                   , CentralPoint(..)
                                   , Point(..)
                                   , UndirectedEdge(..)
                                   , EdgeType(..)
                                   , edgeType
                                   , dupleToEdge)
-- import qualified Cataskell.GameData as GD

type BoardGraph = Gr Point UndirectedEdge
type BoardNodeContext = Context Point UndirectedEdge

-- | Find adjacents and the node itself
getContextByLabel :: Point -> BoardGraph -> Maybe BoardNodeContext
getContextByLabel point gr = listToMaybe $ gsel (\x -> lab' x == point) gr

-- | Get a node based on its point definition.
getNodeMaybe :: Point -> BoardGraph -> Maybe Node 
getNodeMaybe point gr = node' <$> getContextByLabel point gr

-- | Get a node based on its undirected edge description.
getEdgeMaybe :: UndirectedEdge -> BoardGraph -> Maybe (LEdge UndirectedEdge)
getEdgeMaybe edge gr = listToMaybe . filter (\(_, _, x) -> x == edge) $ labEdges gr

-- | Insert a node iff it doesn't yet exist in the graph, then return its index.
insNodeOnce :: Point -> BoardGraph -> (BoardGraph, Node)
insNodeOnce p gr 
  = case getNodeMaybe p gr of 
      Just node -> (gr, node)
      Nothing   -> let n = snd (nodeRange gr) + 1
                       updatedGr = insNode (n, p) gr
                   in  (updatedGr, n)

-- | Insert multiple nodes exactly once.
insNodesOnce :: [Point] -> BoardGraph -> (BoardGraph, [Node])
insNodesOnce ps gr
  = foldl (\(gr', nodes') point -> 
           let (newgr, added) = insNodeOnce point gr'
           in (newgr, nodes' ++ [added])) (gr, []) ps

-- | Insert an edge iff it doesn't yet exist in the graph.
insEdgeOnce :: UndirectedEdge -> BoardGraph -> BoardGraph
insEdgeOnce uEdge gr 
  = case getEdgeMaybe uEdge gr of 
      Just _  -> gr
      Nothing -> insEdge (x, y, uEdge) gr
    where x = fromJust $ getNodeMaybe (point1 uEdge) gr
          y = fromJust $ getNodeMaybe (point2 uEdge) gr

-- | Insert multiple edges iff they don't yet exist in the graph.
insEdgesOnce :: [UndirectedEdge] -> BoardGraph -> BoardGraph
insEdgesOnce uEdges gr
 = foldl (\acc x -> insEdgeOnce x acc) gr uEdges

-- | Add a hex to the graph at specified coordinate, reusing vertices if possible.
addHex :: BoardGraph -> CentralPoint -> BoardGraph
addHex gr centerPoint
  = let (points, uEdges) = mkHexPointsAndEdges centerPoint
        (nodeGr, _) = insNodesOnce points gr
        gr' = insEdgesOnce uEdges nodeGr
    in  undir gr'

connectCenters :: BoardGraph -> BoardGraph
connectCenters = insEdgesOnce centerEdges
  where centerEdges = concatMap (\x -> map dupleToEdge . (zip (repeat x)) $ getNs x) hexCenterPoints
        getNs = (Map.!) hexNeighborhoods

-- | The Catan board as a graph. Occupancy data is stored in a map (see Cataskell.GameData.Board)
boardGraph :: BoardGraph
boardGraph = undir $ connectCenters gr
             where gr = foldl (addHex) empty $ map toCenter hexCenterPoints

-- | Count the nodes that satisfy a predicate.
countNodes :: (Point -> Bool) -> BoardGraph -> Int
countNodes p gr = length $ filter p $ map snd $ labNodes gr

neighborPoints :: BoardGraph -> Point -> [Point]
neighborPoints gr p
  = let thisNode = fromJust $ getNodeMaybe p gr
        nn = neighbors gr thisNode
    in  map snd . filter (\(n, _) -> n `elem` nn) $ labNodes gr

connections :: (EdgeType -> Bool) -> BoardGraph
connections etP = elfilter (etP . edgeType) boardGraph

centerConnections :: BoardGraph
centerConnections = connections (== BetweenCenters)

resourceConnections :: BoardGraph
resourceConnections = connections (== ToCenter)

roadConnections :: BoardGraph
roadConnections = connections (== Between)

allEdges :: [UndirectedEdge]
allEdges = map (\(_, _, x) -> x) $ labEdges roadConnections

boardPrint :: BoardGraph -> IO ()
boardPrint = prettyPrint