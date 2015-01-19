{-# LANGUAGE TemplateHaskell #-}

module Cataskell.Server.MessageTypes where

import Data.Aeson.TH
import Cataskell.SerializeOpts

data AddUser = AddUser String
  deriving (Eq, Show)

deriveFromJSON myOptionsNoLens ''AddUser

data UserAlreadyExists = UserAlreadyExists String
  deriving (Eq, Show)

deriveToJSON myOptionsNoLens ''UserAlreadyExists

data NumConnected = NumConnected 
  { _numUsers :: !Int }
  deriving (Eq, Show)

deriveToJSON myOptions { fieldLabelModifier = drop 1 } ''NumConnected

data NewMessage = NewMessage String
  deriving (Eq, Show)

deriveFromJSON myOptionsNoLens ''NewMessage

data Said = Said 
  { _username :: String
  , _message :: String }
  deriving (Eq, Show)

deriveJSON myOptions ''Said

data UserName = UserName String
  deriving (Eq, Show)

deriveJSON myOptionsNoLens ''UserName

data UserJoined = UserJoined
  { username :: String
  , numUsers :: Int }
  deriving (Eq, Show)

deriveJSON myOptionsNoLens { fieldLabelModifier = id } ''UserJoined
