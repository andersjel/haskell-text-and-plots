{-# LANGUAGE OverloadedStrings #-}
import Text.DocL
import Data.Foldable (fold)
import Data.Aeson (object)
import qualified Data.ByteString.Lazy as B

monthlyRainfall :: [(String, Double)]
monthlyRainfall =
  [ ("Jan", 36.3)
  , ("Feb", 40.9)
  , ("Mar", 42.5)
  , ("Apr", 31.5)
  , ("May", 24.6)
  , ("Jun", 42.1)
  , ("Jul", 42.6)
  , ("Aug", 47.5)
  , ("Sep", 47.2)
  , ("Oct", 37.8)
  , ("Nov", 48.2)
  , ("Dec", 38.6)
  ]

main = B.putStr $ render $ fold
  [ header "Monthly rainfall in Greenland"
  , plot' monthlyRainfall (col' "month" fst) [ col "rainfall (mm)" snd]
      $ object
        [("data", object [("type", "bar")])
        ,("axis", object [("x", object [("type", "category")])])
        ]
  ]
