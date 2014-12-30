-- | Generate a board with connectivity information

module Cataskell.BoardGraph where

import Data.Graph.Inductive
import Data.Graph.Inductive.PatriciaTree()
import Data.Maybe (listToMaybe, fromJust)
import Control.Applicative ((<$>))
import Cataskell.Util
import Cataskell.GameData.Location ( HexCoord
                                   , hexCoords
                                   , VertexPosition(..)
                                   , Point(..)
                                   , UndirectedEdge(..)
                                   , dupleToEdge)
-- import qualified Cataskell.GameData as GD

type BoardGraph = Gr Point UndirectedEdge
type BoardNodeContext = Context Point UndirectedEdge

-- | Vertices around the edge of a given hex
pointsAroundHex :: HexCoord -> [Point]
pointsAroundHex hexCoord
  = let p1 = Point { coord = hexCoord, position = Top }
        p2 = Point { coord = hexCoord + (1, -1), position = Bottom }
        p3 = Point { coord = hexCoord + (0, 1), position = Top }
        p4 = Point { coord = hexCoord, position = Bottom }    
        p5 = Point { coord = hexCoord + (-1, 1), position = Top }
        p6 = Point { coord = hexCoord + (0, -1), position = Bottom }  
    in  [p1, p2, p3, p4, p5, p6]

-- | Find adjacents and the node itself
getContextByLabel :: Point -> BoardGraph -> Maybe BoardNodeContext
getContextByLabel point gr = listToMaybe $ gsel (\x -> lab' x == point) gr

-- | Get a node based on its point definition
getNodeMaybe :: Point -> BoardGraph -> Maybe Node 
getNodeMaybe point gr = node' <$> getContextByLabel point gr

-- | Get a node based on its undirected edge description
getEdgeMaybe :: UndirectedEdge -> BoardGraph -> Maybe (LEdge UndirectedEdge)
getEdgeMaybe edge gr = listToMaybe . filter (\(_, _, x) -> x == edge) $ labEdges gr

-- | Insert a node iff it doesn't yet exist in the graph, then return its index
insNodeOnce :: Point -> BoardGraph -> (BoardGraph, Node)
insNodeOnce p gr 
  = case getNodeMaybe p gr of 
      Just node -> (gr, node)
      Nothing   -> let n = snd (nodeRange gr) + 1
                       updatedGr = insNode (n, p) gr
                   in  (updatedGr, n)

-- | Insert multiple nodes exactly once
insNodesOnce :: [Point] -> BoardGraph -> (BoardGraph, [Node])
insNodesOnce ps gr
  = foldl (\(gr', nodes') point -> 
           let (newgr, added) = insNodeOnce point gr'
           in (newgr, nodes' ++ [added])) (gr, []) ps


-- | Insert an edge iff it doesn't yet exist in the graph
insEdgeOnce :: UndirectedEdge -> BoardGraph -> BoardGraph
insEdgeOnce uEdge gr 
  = case getEdgeMaybe uEdge gr of 
      Just _  -> gr
      Nothing -> insEdge (x, y, uEdge) gr
    where x = fromJust $ getNodeMaybe (point1 uEdge) gr
          y = fromJust $ getNodeMaybe (point2 uEdge) gr

insEdgesOnce :: [UndirectedEdge] -> BoardGraph -> BoardGraph
insEdgesOnce uEdges gr
 = foldl (\acc x -> insEdgeOnce x acc) gr uEdges

mkHexGraph :: HexCoord -> ([Point], [UndirectedEdge])
mkHexGraph hexCoord
  = let center = Point { coord = hexCoord, position = Center }
        edgePoints = pointsAroundHex hexCoord
        allPoints = center:edgePoints
        toCenters = map dupleToEdge $ zip (repeat center) edgePoints
        loop = windowed 2 (edgePoints ++ [head edgePoints])
        loop' = map (dupleToEdge . fromJust . listToDuple) loop
        allEdges = loop' ++ toCenters
    in (allPoints, allEdges)

-- | Add a hex to the graph at specified coordinate, reusing vertices if possible
addHex :: HexCoord -> BoardGraph -> BoardGraph
addHex hexCoord gr
  = let (points, uEdges) = mkHexGraph hexCoord
        (nodeGr, _) = insNodesOnce points gr
        gr' = insEdgesOnce uEdges nodeGr
    in  undir gr'

-- | The Catan board as a graph. Occupancy data is stored elsewhere. Nodes 1-19 are the hexCoords
boardGraph :: BoardGraph
boardGraph = foldl (flip addHex) start hexCoords
             where start = fst $ insNodesOnce initialPoints empty
                   initialPoints = map (\x -> Point { coord = x, position = Center }) hexCoords

-- | Count the nodes that satisfy a predicate
countNodes :: (Point -> Bool) -> BoardGraph -> Int
countNodes p gr = length $ filter p $ map snd $ labNodes gr

neighborPoints :: Point -> [Point]
neighborPoints = undefined

boardPrint :: BoardGraph -> IO ()
boardPrint = prettyPrint