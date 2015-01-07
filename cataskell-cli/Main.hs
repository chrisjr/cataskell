import Cataskell.Game
import Control.Monad.Random
import Control.Monad.State
import Control.Lens
import System.Random

main :: IO ()
main = do
  let (initialGame, r') = runRand (newGame ["1", "2", "3", "4"]) (mkStdGen 0)
  let gs = iterate (\(x, r) -> runRand (execStateT randomAct x) r) (initialGame, r')
  let gsActions = map (view lastAction . fst) $ take 100 gs
  forM_ gsActions print
