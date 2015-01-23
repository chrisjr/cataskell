{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings #-}

module Cataskell.Server.Main where

import Control.Applicative
import Cataskell.Server.App (server, ServerState (..), startingState)
import Network.Wai (Application)
import qualified Yesod.Core as YC
import qualified Control.Concurrent.STM as STM
import qualified Network.SocketIO as SocketIO
import qualified Network.EngineIO.Yesod as EIOYesod
import Data.Maybe (maybe)
import System.Environment (lookupEnv)

import Paths_cataskell (getDataDir)

data YesodChat = YesodChat { socketIoHandler :: YC.HandlerT YesodChat IO () }

YC.mkYesod "YesodChat" [YC.parseRoutesNoCheck|
/ IndexR GET
/main.js MainJSR GET
/style.css StyleCSSR GET
/socket.io/ SocketIOR
|]

instance YC.Yesod YesodChat where
  -- do not redirect /socket.io/?bla=blub to /socket.io?bla=blub
  cleanPath _ ["socket.io",""] = Right ["socket.io"]
  cleanPath _ p = Right p

getIndexR :: Handler ()
getIndexR = do
  dataDir <- YC.liftIO getDataDir
  YC.sendFile "text/html" $ dataDir ++ "/index.html"

getStyleCSSR :: Handler ()
getStyleCSSR = do
  dataDir <- YC.liftIO getDataDir
  YC.sendFile "text/css" $ dataDir ++ "/style.css"

getMainJSR :: Handler ()
getMainJSR = do
  dataDir <- YC.liftIO getDataDir
  YC.sendFile "application/javascript" $ dataDir ++ "/main.js"

handleSocketIOR :: Handler ()
handleSocketIOR = YC.getYesod >>= socketIoHandler

site :: IO YesodChat
site = do
  state <- startingState
  YesodChat <$> SocketIO.initialize EIOYesod.yesodAPI (server state)

app :: IO Application
app = do
  site' <- site
  YC.toWaiApp site'

serverMain :: IO ()
serverMain = do
  site' <- site
  portNo <- lookupEnv "PORT"
  YC.warp (maybe 8080 read portNo) site'
