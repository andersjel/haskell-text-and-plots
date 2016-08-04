import Text.DocL (render, header, text, plot, line, lineName, labelX)
import Data.Foldable (fold)
import qualified Data.ByteString.Lazy as B
import Data.Function ((&))

main = B.putStr $ render $ fold
    [ header "Stirling's approximation"
    , text $ fold
      [ "A simple approximation of the factorial function is given by "
      , "Stirling's approximation." ]
    , plot
      [ lineName "log(n!)"     $ line ns (map logfact ns)
      , lineName "n log n - n" $ line ns (map stirling ns) ]
      [ labelX "n" ]
    ]
  where
    ns = [1..50]
    logfact n = sum $ map log [1..n]
    stirling n = n * log n - n
