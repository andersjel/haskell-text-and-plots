import Text.DocL
import Text.DocL.Javascript
import Data.Foldable (fold)
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
  , plotRows' monthlyRainfall (field' "month" fst) [field "rainfall (mm)" snd]
      [ "data" /: ["type" /: "bar"]
      , "axis" /:
        [ "x" /: ["type" /: "category"]]
      ]
  ]
