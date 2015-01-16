{-# LANGUAGE OverloadedStrings #-}
import Cataskell.Game
import Cataskell.Serialize
import Control.Monad.Random
import Control.Monad.State
import Control.Lens
import System.Random
import System.Environment (getArgs)
import Control.Exception (assert)
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import System.IO (openBinaryFile, IOMode(WriteMode), hClose)

getToEnd :: Int -> Int -> Int -> (Int, [Game], Bool)
getToEnd maxIter maxSeed seed
  = let (initialGame, r') = runRand (newGame ["1","2","3"]) (mkStdGen seed)
        gs = iterate (\(x, r) -> runRand (execStateT randomActGoodInitial x) r) (initialGame, r')
        maxIterGs = take maxIter . takeWhile ((/= End) . view phase) $ map fst gs
    in if seed < maxSeed
       then if view phase (last maxIterGs) == End
              then (seed, maxIterGs, True)
              else getToEnd maxIter maxSeed (seed+1)
       else (seed, maxIterGs, False) 

main :: IO ()
main = do
  args <- getArgs
  let (iters, maxSeed) = case args of
                                       [] -> (1000, 1000)
                                       [i] -> (read i, 1000)
                                       (i:s:_) -> (read i, read s)
  let (seed, allGs, success) = getToEnd iters maxSeed 0
  print (seed, success)
  let gs' = encode allGs
  h <- openBinaryFile "games.js" WriteMode
  B.hPut h "var data="
  B.hPut h gs'
  B.hPut h ";"
  hClose h
