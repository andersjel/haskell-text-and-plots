# The text-and-plots package
*text-and-plots* is a Haskell EDSL to create documents with embedded plots.

Example usage:
```haskell
{-# LANGUAGE OverloadedStrings #-}
import Text.DocL
import Data.Foldable (fold)

main = renderToFile "out.html" $ fold
  [ header "Stirling's approximation"
  , text $ fold
      [ "A simple approximation of the factorial function is given by "
      , "Stirling's approximation." ]
  , plot [1..50] (col "n" id)
      [ col "log(n!)" $ \n -> sum $ map log [1..n]
      , col "n log n - n" $ \n -> n * log n - n ]
  ]
```

## Details

The `render` function converts the document to HTML with plots based on the [C3.js libray](http://c3js.org/). *text-and-plots* does not try to wrap every aspect of the *C3.js* library, but instead exposes it directly if needed.
