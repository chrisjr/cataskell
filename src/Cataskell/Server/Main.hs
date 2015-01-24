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
import Data.ByteString (ByteString)
import Data.Maybe (maybe)
import Data.Monoid (mempty)
import Control.Monad (liftM)
import Control.Monad.IO.Class (liftIO)
import System.Environment (lookupEnv)

import Paths_cataskell (getDataDir)

data App = App
  { _serverState :: ServerState
  }

makeLenses ''App

socketIoHandler :: IO (Snap.Handler App App ())
socketIoHandler = do
  state <- startingState
  SocketIO.initialize EIOSnap.snapAPI (server state)

staticFiles :: Snap.Handler App App ()
staticFiles = liftIO getDataDir >>= Snap.serveDirectory

routes :: IO [(ByteString, Snap.Handler App App ())]
routes = do
  handler <- socketIoHandler
  return [ ("/socket.io", handler)
         , ("/", staticFiles)
         ]

app :: Snap.SnapletInit App App
app = Snap.makeSnaplet "app" "Cataskell websocket server" (Just getDataDir) $ do
  s <- liftIO startingState
  routes' <- liftIO routes
  Snap.addRoutes routes'
  return $ App s

config :: IO (Snap.Config m a)
config = do
  portNo <- lookupEnv "PORT"
  return $ Snap.setPort (maybe 8080 read portNo) mempty

serverMain :: IO ()
serverMain = do
  config' <- liftIO config
  config'' <- Snap.commandLineConfig config'
  Snap.serveSnaplet config'' app

