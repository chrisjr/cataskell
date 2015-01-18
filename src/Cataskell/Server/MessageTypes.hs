{-# LANGUAGE TemplateHaskell #-}

module Cataskell.Server.MessageTypes where

import Data.Aeson.TH
import Cataskell.SerializeOpts
import qualified Data.Text as Text

data AddUser = AddUser Text.Text
  deriving (Eq, Show)

deriveFromJSON myOptionsNoLens ''AddUser

data NumConnected = NumConnected 
  { _numUsers :: !Int }
  deriving (Eq, Show)

deriveToJSON myOptions { fieldLabelModifier = drop 1 } ''NumConnected

data NewMessage = NewMessage Text.Text
  deriving (Eq, Show)

deriveFromJSON myOptionsNoLens ''NewMessage

data Said = Said 
  { _username :: Text.Text
  , _message :: Text.Text }
  deriving (Eq, Show)

deriveJSON myOptions ''Said

data UserName = UserName Text.Text
  deriving (Eq, Show)

deriveJSON myOptionsNoLens ''UserName

data UserJoined = UserJoined
  { username :: Text.Text
  , numUsers :: Int }
  deriving (Eq, Show)

deriveJSON myOptionsNoLens { fieldLabelModifier = id } ''UserJoined
