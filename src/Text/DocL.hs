{-|
Module      : Text.DocL
Copyright   : 2015 Anders Jellinggaard
License     : MIT
Maintainer  : anders.jel@gmail.com
Stability   : experimental

Haskell mini-language to create HTML documents with a mixture of markup and
plots based on the <http://c3js.org/ C3.js> library.

See the <https://github.com/andersjel/haskell-text-and-plots github page> for an
introduction.

Notes:

* The generated documents relies on <https://cdnjs.com/> to serve
  <http://c3js.org/ C3.js> and friends.

* This library should not be used on a public facing server with input supplied
  by the user. The library has not been properly vetted for security issues.
-}

module Text.DocL (
  Doc,
  -- * Markup
  text, header, markdown, html,
  -- * Output
  render, renderToFile,
  -- * Plotting
  plot, line,
  -- ** Modifying a line
  lineType, lineName, lineColor, lineAdjust,
  -- ** Plot options
  labelX, labelY, hidePoints, height, width, option,
  -- * Advanced plotting
  plotRaw,
  -- * Utilities
  linspace,
  -- * Re-exports from "Text.DocL.Javascript"
  (/:), javascript
  ) where

import Control.Exception             (assert)
import Data.Foldable
import Data.HashMap.Strict           (insert, unionWith)
import Data.List                     (transpose)
import Data.Monoid
import Data.Sequence                 (Seq, singleton)
import Data.String                   (IsString (..))
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Blaze.Html5              ((!))
import Text.DocL.Javascript

import qualified Data.ByteString.Lazy        as B
import qualified System.IO                   as IO
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Markdown               as Markdown

data Node = Chart [Prop] | Html H.Html

-- | Base type representing a document. This type allows composition via the
-- 'Monoid' instance.
newtype Doc = Doc (Seq Node) deriving Monoid

data Line = Line {_line :: String -> String -> [Prop]}

data PlotOption = PlotOption {_plotOption :: [Prop]}

plot :: [Line] -> [PlotOption] -> Doc
plot items options =
  let
    itemProps = concatMap itemProp $ zip [0..] items
    itemProp (n, Line  f) = f ('x' : show n) ('y' : show n)
  in
    plotRaw $ itemProps ++ concatMap _plotOption options

line :: [Double] -> [Double] -> Line
line xs ys =
  Line $ \x y ->
    [ "data" /:
      [ "json" /: [x /: xs, y /: ys]
      , "xs"   /: [y /: x]
      ]
    ]

lineAdjust :: (String -> String -> [Prop]) -> Line -> Line
lineAdjust f (Line g) = Line $ \x y -> g x y ++ f x y

lineType :: String -> Line -> Line
lineType t = lineAdjust $ \_ y -> ["data" /: ["types" /: [y /: t]]]

lineName :: String -> Line -> Line
lineName n = lineAdjust $ \_ y -> ["data" /: ["names" /: [y /: n]]]

lineColor :: String -> Line -> Line
lineColor c = lineAdjust $ \_ y -> ["data" /: ["colors" /: [y /: c]]]

option :: [Prop] -> PlotOption
option = PlotOption

labelX, labelY :: String -> PlotOption
labelX s = option ["axis" /: ["x" /: ["label" /: s]]]
labelY s = option ["axis" /: ["y" /: ["label" /: s]]]

hidePoints :: PlotOption
hidePoints = option ["point" /: ["show" /: False]]

height, width :: Int -> PlotOption
height n = option ["size" /: ["height" /: n]]
width n = option ["size" /: ["width" /: n]]

-- | This function sends an object with the supplied properties directly to
-- <http://c3js.org/ C3.js>. This is the do-it-yourself option which exposes all
-- of <http://c3js.org/ C3.js>. The object receives a
-- <http://c3js.org/reference.html#bindto "bindto"> property,
-- targeting a @\<div\>@ tag placed appropriately.
plotRaw :: [Prop] -> Doc
plotRaw = Doc . singleton . Chart

-- | Creates a 'Doc' representing raw html.
html :: H.Html -> Doc
html = Doc . singleton . Html

-- | Creates a 'Doc' representing plain text. The string will be properly
-- escaped.
text :: String -> Doc
text = html . H.p . H.toMarkup

-- | Creates a 'Doc' representing @\<h1\>@ header tag. The string will be
-- properly escaped.
header :: String -> Doc
header = html . H.h1 . H.toMarkup

-- | Converts a string in
-- <http://en.wikipedia.org/wiki/Markdown markdown syntax> to a 'Doc'.
markdown :: String -> Doc
markdown = html . Markdown.markdown Markdown.def . fromString

-- | Render a document to a lazy 'B.ByteString'.
render :: Doc -> B.ByteString
render (Doc doc) = renderHtml html
  where
    dns = "https://cdnjs.cloudflare.com/ajax/libs/"
    html = H.docTypeHtml $ do
      H.head $ do
        H.link ! A.rel "stylesheet" ! A.href (dns <> "c3/0.4.9/c3.min.css")
        H.script ! A.charset "utf-8" ! A.src (dns <> "d3/3.5.3/d3.min.js") $
          mempty
        H.script ! A.charset "utf-8" ! A.src (dns <> "c3/0.4.9/c3.min.js") $
          mempty
      H.body $
        foldMap f (zip [(0::Int)..] $ toList doc)
    f (_, (Html t)) = t
    f (i, (Chart props)) = divTag <> scriptTag
      where
        name = "plot" <> show i
        divTag = H.div ! A.id (fromString name) $ mempty
        obj = props ++ ["bindto" /: ("#" ++ name)]
        js = encode $ toObj obj
        scriptTag = H.script ! A.type_ "text/javascript" $
          H.preEscapedToMarkup $ "\nc3.generate(" <> js <> ");\n"

-- | If the file exists, it will be overwritten.
renderToFile :: FilePath -> Doc -> IO ()
renderToFile p = B.writeFile p . render

-- | /'linspace' x y n/ generates /n/ evenly spaced values from /x/ to /y/.
linspace :: Fractional a => a -> a -> Int -> [a]
linspace x y n = [x + (fromIntegral i)*d | i <- [0 .. n - 1]]
  where d = (y - x) / (fromIntegral n - 1)
