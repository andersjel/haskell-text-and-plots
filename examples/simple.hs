import Text.DocL (render, header, text, plotRows, field)
import Data.Foldable (fold)
import qualified Data.ByteString.Lazy as B

main = B.putStr $ render $ fold
  [ header "Stirling's approximation"
  , text $ fold
      [ "A simple approximation of the factorial function is given by "
      , "Stirling's approximation." ]
  , plotRows [1..50] (field "n" id)
      [ field "log(n!)" $ \n -> sum $ map log [1..n]
      , field "n log n - n" $ \n -> n * log n - n ]
  ]
