{-# LANGUAGE OverloadedStrings #-}
module Cataskell.Server.Main where

import System.Environment
import Network.Wai
import Network.Wai.Handler.Warp (run)

import Cataskell.Server.App (app)

serverMain :: IO ()
serverMain = do
  port <- getEnv "PORT"
  run (fromIntegral $ read port) app
