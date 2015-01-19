{-# LANGUAGE OverloadedStrings #-}
module Cataskell.Server.MessageTypesSpec (main, spec) where

import Test.Hspec
import Cataskell.Server.MessageTypes
import Data.Aeson

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $
  describe "Message types" $ do
    describe "AddUser" $
      it "can be decoded from a string" $ do
        let obj = AddUser "Test"
        let obj' = fromJSON "Test" :: Result AddUser
        obj' `shouldBe` Success obj
    describe "NewMessage" $
      it "contains a message from the user" $ do
        let obj = NewMessage "Hello world."
        let obj' = fromJSON "Hello world." :: Result NewMessage
        obj' `shouldBe` Success obj
    describe "UserName" $
      it "contains a username" $ do
        let obj = UserName "Test2"
        let obj' = fromJSON "Test2" :: Result UserName
        obj' `shouldBe` Success obj
    describe "UserJoined" $
      it "contains a username and the current number of users" $ do
        let obj = UserJoined "Joiner" 3
        let obj' = decode "{\"username\": \"Joiner\", \"numUsers\": 3}" :: Maybe UserJoined
        obj' `shouldBe` Just obj
