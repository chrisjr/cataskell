module Cataskell.SerializeOpts where

import Data.Aeson.TH
import Data.Char

myOptions :: Options
myOptions = defaultOptions { fieldLabelModifier = drop 1
                           , constructorTagModifier = map toLower
                           , omitNothingFields = True
                           , sumEncoding = ObjectWithSingleField }

myOptionsNoLens :: Options
myOptionsNoLens = myOptions { fieldLabelModifier = id }
