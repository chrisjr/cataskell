{-# LANGUAGE OverloadedStrings #-}
import Cataskell.Game
import Cataskell.Serialize
import Control.Monad.Random
import Control.Monad.State
import Control.Lens
import System.Random
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import System.IO (openBinaryFile, IOMode(WriteMode), hClose)

main :: IO ()
main = do
  let (initialGame, r') = runRand (newGame ["1", "2", "3", "4"]) (mkStdGen 1)
  let gs = iterate (\(x, r) -> runRand (execStateT randomAct x) r) (initialGame, r')
  -- let gsActions = map (view lastAction . fst) $ take 100 gs
  -- forM_ gsActions print
  let gs' = encode . map fst $ take 100 gs
  h <- openBinaryFile "games.js" WriteMode
  B.hPut h "var data="
  B.hPut h gs'
  B.hPut h ";"
  hClose h
