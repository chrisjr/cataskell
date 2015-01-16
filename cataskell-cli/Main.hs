{-# LANGUAGE OverloadedStrings #-}
import Cataskell.Game
import Cataskell.Serialize
import Control.Monad.Random
import Control.Monad.State
import Control.Lens
import System.Random
import System.Environment (getArgs)
import Control.Exception (assert)
import Data.List (findIndex)
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import System.IO (openBinaryFile, IOMode(WriteMode), hClose)

getToEnd :: Int -> Int -> Int -> (Int, [Game], Bool)
getToEnd maxIter maxSeed seed
  = let (initialGame, r') = runRand (newGame ["1","2","3"]) (mkStdGen seed)
        gs = map fst $ iterate (\(x, r) -> runRand (execStateT randomActGoodInitial x) r) (initialGame, r')
        endsAt = findIndex ((== End) . view phase) gs
    in if seed < maxSeed
       then maybe (getToEnd maxIter maxSeed (seed+1)) (\end -> (seed, take (end+1) gs, True)) endsAt
       else (seed, take maxIter gs, False) 

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
