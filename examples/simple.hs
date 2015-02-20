import Text.DocL (render, header, text, plot, col)
import Data.Foldable (fold)
import qualified Data.ByteString.Lazy as B

main = B.putStr $ render $ fold
  [ header "Stirling's approximation"
  , text $ fold
      [ "A simple approximation of the factorial function is given by "
      , "Stirling's approximation." ]
  , plot [1..50] (col "n" id)
      [ col "log(n!)" $ \n -> sum $ map log [1..n]
      , col "n log n - n" $ \n -> n * log n - n ]
  ]
