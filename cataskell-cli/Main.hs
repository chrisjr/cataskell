import Cataskell.BoardGraph
import Cataskell.GameData.Location
import Data.Graph.Inductive.Dot
import Data.Graph.Inductive.Basic

main :: IO ()
main = do
  let dot = showDot (fglToDot $ elfilter (\x -> edgeType x == Between) boardGraph)
  writeFile "board.dot" dot
