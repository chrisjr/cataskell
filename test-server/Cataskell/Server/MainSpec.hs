{-# LANGUAGE OverloadedStrings #-}
module Cataskell.Server.MainSpec (main, spec) where

import Test.Hspec
import Test.Hspec.Snap
import Snap (route)
import Cataskell.Server.Main (app, routes, startingState)

main :: IO ()
main = hspec spec

spec :: Spec
spec = snap (route routes) app $ do
  describe "GET /" $
    it "should serve an HTML page" $
      get "/" >>= should200
  describe "GET /main.js" $
    it "should return JavaScript" $
      get "/main.js" >>= should200
  describe "GET /style.css" $
    it "should return CSS" $
      get "/style.css" >>= should200
