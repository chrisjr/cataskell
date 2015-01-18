{-# LANGUAGE OverloadedStrings #-}
module Cataskell.Server.App where

import Control.Exception (bracket_)
import Network.Wai
import Network.HTTP.Types (status200)

app :: Application
app _ respond = bracket_
  (putStrLn "allocating resources")
  (putStrLn "cleaning up")
  (respond $ responseLBS status200 [("Content-Type", "text/plain")] "Hello world.")
