{-# LANGUAGE OverloadedStrings #-}
module Cataskell.Server.MainSpec (main, spec) where

import Test.Hspec
import Test.Hspec.Wai
import Cataskell.Server.Main (app)

main :: IO ()
main = hspec spec

spec :: Spec
spec = with app $ do
  describe "GET /" $
    it "should serve an HTML page" $
      get "/" `shouldRespondWith` 200 {matchHeaders = ["Content-Type" <:> "text/html"]}
  describe "GET /main.js" $
    it "should return JavaScript" $
      get "/main.js" `shouldRespondWith` 200 {matchHeaders = ["Content-Type" <:> "application/javascript"]}
  describe "GET /style.css" $
    it "should return CSS" $
      get "/style.css" `shouldRespondWith` 200 {matchHeaders = ["Content-Type" <:> "text/css"]}
