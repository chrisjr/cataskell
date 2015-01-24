{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cataskell.Server.App (server, ServerState (..), startingState) where

import Prelude hiding (mapM_)

import Control.Lens
import Control.Monad (when)
import Control.Monad.Reader (ask, MonadReader)
import Control.Monad.State.Class (MonadState)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Aeson (ToJSON)
import Data.Text (Text)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Foldable (mapM_, forM_)
import Control.Applicative
import Cataskell.Game (Game, newGame, runGame, update, players)
import Cataskell.GameData.Actions (PlayerAction, GameAction(..))
import Cataskell.GameData.Player (PlayerIndex, playerName)
import Cataskell.GameData.PlayerView (PlayerView, viewFor)
import Cataskell.Server.MessageTypes
import Cataskell.Serialize()
import System.Random
import Control.Monad.Random
import qualified Control.Concurrent.STM as STM
import qualified Data.Aeson as Aeson()
import qualified Network.SocketIO as SocketIO

--------------------------------------------------------------------------------
data ServerState = ServerState
  { ssUserNamesToSockets :: STM.TVar (Map String SocketIO.Socket)
  , ssNVotedToStart :: STM.TVar Int
  , ssGameStdGen :: STM.TMVar (Game, StdGen)
  , ssPlayerIndexes :: STM.TVar (Map PlayerIndex (Maybe SocketIO.Socket))
  , ssLastStates :: STM.TVar (Map SocketIO.Socket PlayerView) }

startingState :: IO ServerState
startingState = do
  userNamesToSocketId <- STM.newTVarIO Map.empty
  nVotedToStart <- STM.newTVarIO 0
  gameStdGen <- STM.newEmptyTMVarIO
  playerIndexes <- STM.newTVarIO Map.empty
  lastStates <- STM.newTVarIO Map.empty
  return $ ServerState userNamesToSocketId nVotedToStart gameStdGen playerIndexes lastStates

fillPlayers :: ServerState -> STM.STM ()
fillPlayers state = do
  mGameStdGen <- STM.tryReadTMVar (ssGameStdGen state)
  sockets <- STM.readTVar (ssUserNamesToSockets state)
  case mGameStdGen of
    Just (g, _)-> do
      let pItoNames = Map.map (^.playerName) (g^.players)
      when (Map.size pItoNames == Map.size sockets) $ do
        let pItoSocket = Map.map (`Map.lookup` sockets) pItoNames
        STM.writeTVar (ssPlayerIndexes state) pItoSocket
    Nothing -> return ()

flipMaybeMap :: (Ord a) => Map k (Maybe a) -> Map a k
flipMaybeMap = Map.fromList . mapMaybe (\(x,my) -> fmap (\y -> (y, x)) my) . Map.toList

getGameStates :: (MonadIO m) => ServerState -> m (Map SocketIO.Socket PlayerView)
getGameStates state = liftIO $ STM.atomically $ do
  mGameStdGen <- STM.tryReadTMVar (ssGameStdGen state)
  case mGameStdGen of
    Just (g, _) -> do
      playersToSockets <- STM.readTVar (ssPlayerIndexes state)
      let socketsToPlayers = flipMaybeMap playersToSockets
      let socketsToStates = Map.map (`viewFor` g) socketsToPlayers
      return socketsToStates
    Nothing -> return Map.empty

-- | new map -> old map -> map of changed elements
onlyNew :: (Eq a, Ord k) => Map k a -> Map k a -> Map k a
onlyNew = Map.differenceWith f
  where f new old = if new /= old then Just new else Nothing

updateAndGetUpdatedStates :: (MonadIO m) => ServerState -> m [(SocketIO.Socket, PlayerView)]
updateAndGetUpdatedStates state = do
  statesToSendMap <- getGameStates state
  liftIO $ STM.atomically $ do
    lastStates <- STM.readTVar (ssLastStates state)
    STM.writeTVar (ssLastStates state) statesToSendMap -- update
    let statesToSendMap' = onlyNew statesToSendMap lastStates
    return $ Map.toList statesToSendMap'

sendGameStateToPlayers :: (MonadIO m) => ServerState -> m ()
sendGameStateToPlayers state = do
  statesToSend <- updateAndGetUpdatedStates state
  forM_ statesToSend $ \(sock, gState) -> SocketIO.emitTo sock "game state" gState

forMAtomic_ :: (MonadIO m) => STM.TMVar a -> (a -> m b) -> m ()
forMAtomic_ mvar m
  = liftIO (STM.atomically (STM.tryReadTMVar mvar)) >>= mapM_ m 

broadcastMessageEvent :: (ToJSON a, MonadReader SocketIO.Socket m, MonadIO m) 
  => Text
  -> (String -> messageType -> a) 
  -> messageType
  -> ((String -> m ()) -> SocketIO.EventHandler ())
  -> SocketIO.EventHandler ()
broadcastMessageEvent eventName f x forUserName
  = forUserName $ \userName ->
      SocketIO.broadcast eventName (f userName x)

newMessageHandler :: (MonadIO m, MonadReader SocketIO.Socket m)
  => ((String -> m ()) -> SocketIO.EventHandler ()) 
  -> NewMessage
  -> SocketIO.EventHandler ()
newMessageHandler forUserName (NewMessage message)
  = broadcastMessageEvent "new message" Said message forUserName

typingHandler :: (MonadReader SocketIO.Socket m, MonadIO m)
  => ((String -> m ()) -> SocketIO.EventHandler ())
  -> SocketIO.EventHandler ()
typingHandler = broadcastMessageEvent "typing" (\userName _ -> (UserName userName)) ()

stopTypingHandler :: (MonadReader SocketIO.Socket m, MonadIO m)
  => ((String -> m ()) -> SocketIO.EventHandler ())
  -> SocketIO.EventHandler ()
stopTypingHandler = broadcastMessageEvent "stop typing" (\userName _ -> (UserName userName)) ()

addUserHandler :: (MonadIO m, MonadReader SocketIO.Socket m) 
  => ServerState
  -> STM.TMVar String
  -> AddUser
  -> m ()
addUserHandler state userNameMVar (AddUser userName) = do
  mySocket <- ask
  mUserNamesToSocketId <- liftIO $ STM.atomically $ do
    let toSockets = ssUserNamesToSockets state
    STM.putTMVar userNameMVar userName
    userNamesToSockets <- STM.readTVar toSockets
    case Map.lookup userName userNamesToSockets of
      Nothing -> do
        let result = Map.insert userName mySocket userNamesToSockets
        STM.writeTVar toSockets result
        return $ Just result
      Just x -> if x == mySocket
                   then return $ Just userNamesToSockets
                   else return Nothing
  case mUserNamesToSocketId of
    Nothing -> SocketIO.emit "error" (UserAlreadyExists userName)
    Just userNamesMap -> do
      let n = Map.size userNamesMap
      SocketIO.emit "login" (NumConnected n)
      SocketIO.broadcast "user joined" (UserJoined userName n)

disconnectHandler :: (MonadIO m, MonadReader SocketIO.Socket m) 
  => ServerState
  -> STM.TMVar String
  -> m ()
disconnectHandler state userNameMVar = do
    mySocket <- ask
    (userNamesToSockets, mUserName) <- liftIO $ STM.atomically $ do
      mUserName <- STM.tryReadTMVar userNameMVar
      let toSockets = ssUserNamesToSockets state
      userNamesToSockets <- STM.readTVar toSockets
      case mUserName of
        Nothing -> return (userNamesToSockets, mUserName)
        Just userName -> do
          let sock = Map.lookup userName userNamesToSockets
          case sock of
            Just s | s == mySocket -> do 
              let updated = Map.delete userName userNamesToSockets
              STM.writeTVar toSockets updated
              return (updated, mUserName)
            _ -> return (userNamesToSockets, Nothing)
    case mUserName of
      Nothing -> return ()
      Just userName ->
        SocketIO.broadcast "user left" (UserJoined userName (Map.size userNamesToSockets))

startGameHandler :: (MonadReader SocketIO.Socket m, MonadIO m)
  => ServerState
  -> ((String -> m ()) -> SocketIO.EventHandler ())
  -> SocketIO.EventHandler ()
startGameHandler state forUserName = do
  let toSockets = ssUserNamesToSockets state
  (userNamesToSockets, nStarted) <- liftIO $ STM.atomically $ do
    nStarted <- (+ 1) <$> STM.readTVar (ssNVotedToStart state)
    userNamesToSockets <- STM.readTVar toSockets
    STM.writeTVar (ssNVotedToStart state) nStarted
    return (userNamesToSockets, nStarted)
  broadcastMessageEvent "vote to start" (\userName _ -> (UserName userName)) () forUserName
  when (nStarted >= 3 && nStarted == Map.size userNamesToSockets) $ do
    stdGen <- liftIO getStdGen
    liftIO $ STM.atomically $ do
      userNamesToSockets' <- STM.readTVar toSockets
      let names = Map.keys userNamesToSockets'
      let (game, stdGen') = runRand (newGame names) stdGen
      STM.putTMVar (ssGameStdGen state) (game, stdGen')
      fillPlayers state
    sendGameStateToPlayers state

playerActionHandler :: (MonadIO m, MonadReader SocketIO.Socket m) 
  => ServerState
  -> PlayerAction
  -> m ()
playerActionHandler state x = do
  mySocket <- ask
  liftIO $ STM.atomically $ do
    playersToSockets <- STM.readTVar (ssPlayerIndexes state)
    let socketsToPlayers = flipMaybeMap playersToSockets
    let pImaybe = Map.lookup mySocket socketsToPlayers
    case pImaybe of
      Just pI -> do
        let act' = PlayerAction pI x
        mGameStdGen <- STM.tryReadTMVar (ssGameStdGen state)
        case mGameStdGen of
          Just (g, stdGen) -> do
            let (g', stdGen') = runGame (update act') g stdGen
            STM.putTMVar (ssGameStdGen state) (g', stdGen')
          Nothing -> return ()
      Nothing -> return ()
  sendGameStateToPlayers state

server :: (MonadState SocketIO.RoutingTable m, MonadIO m, Applicative m) => ServerState -> m ()
server state = do
  userNameMVar <- liftIO STM.newEmptyTMVarIO
  let forUserName = forMAtomic_ userNameMVar

  SocketIO.on "new message" (newMessageHandler forUserName)

  SocketIO.on "add user" (addUserHandler state userNameMVar)

  SocketIO.appendDisconnectHandler (disconnectHandler state userNameMVar)

  SocketIO.on "typing" (typingHandler forUserName)
  SocketIO.on "stop typing" (stopTypingHandler forUserName)

  SocketIO.on "start game" (startGameHandler state forUserName)
  SocketIO.on "action" (playerActionHandler state)
