# The text-and-plots package
*text-and-plots* is a Haskell EDSL to create documents with embedded plots.

Example usage:
```haskell
import Text.DocL
main = undefined
```

## Details

The `render` function converts the document to HTML with plots based on the [C3.js libray](http://c3js.org/). *text-and-plots* does not try to wrap every aspect of the *C3.js* library, but instead exposes it directly.
