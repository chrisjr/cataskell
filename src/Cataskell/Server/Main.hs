{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings #-}

module Cataskell.Server.Main where

import Control.Applicative
import Control.Lens
import Cataskell.Server.App (server, ServerState (..), startingState)
import qualified Snap.Core as Snap
import qualified Snap.Snaplet as Snap
import qualified Snap.Util.FileServe as Snap
import qualified Snap.Http.Server as Snap
import qualified Control.Concurrent.STM as STM
import qualified Network.SocketIO as SocketIO
import qualified Network.EngineIO.Snap as EIOSnap
import Data.Maybe (maybe)
import Data.Monoid (mempty)
import System.Environment (lookupEnv)

import Paths_cataskell (getDataDir)

data App = App
  { _serverState :: ServerState
  }

makeLenses ''App

routes = do
  state <- startingState
  socketIoHandler <- SocketIO.initialize EIOSnap.snapAPI (server state)
  dataDir <- getDataDir
  return [ ("/socket.io", socketIoHandler)
         , ("/", Snap.serveDirectory dataDir)
         ]

app :: IO (Snap.SnapletInit App App)
app = Snap.makeSnaplet "app" "Cataskell websocket server" (Just getDataDir) $ do
  s <- startingState
  Snap.addRoutes routes
  return $ App s

config = do
  portNo <- lookupEnv "PORT"
  return $ Snap.setPort (maybe 8080 read portNo) mempty

serverMain :: IO ()
serverMain = do
  config' <- Snap.commandLineConfig config
  Snap.serveSnaplet config app

