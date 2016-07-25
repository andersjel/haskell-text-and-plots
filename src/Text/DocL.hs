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
  -- * Row-wise plotting
  Field, field, field',
  plotRows, plotRows',
  -- * Column-wise plotting
  Column, col, col',
  plotCols, plotCols',
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

-- | Represent a column of a dataset where each row has type /a/. See 'field'
-- for details.
data Field a = Field
  { _fieldHeader :: String
  , _extract     :: a -> Obj
  }

-- | Polymorphic version of 'field'. This allows, for instance, 'Int' and 'String'
-- values to be used in plots. It is up to the caller to ensure that the values
-- make sense to <http://c3js.org/ C3.js>.
field' :: ToObj b => String -> (a -> b) -> Field a
field' h f = Field h (toObj . f)

-- | Datasets are supplied to 'plotRows' as a list of values. This function
-- constructs a 'Field' which knows how to extract one 'Double' from each
-- value in this list.
--
-- The first argument to 'field' is the header of the column, which is used in
-- the legend of the plot.
--
-- The simplest plot that can be created consist of one column for the /x/
-- values and one column for the /y/ values:
--
-- @
--  -- Plot x² vs x.
--  plotRows [1..10] (field "x" id) [field "x²" $ \\x -> x*x]
-- @
field :: String -> (a -> Double) -> Field a
field = field'

-- | > plotRows ds x ys
--
-- Plots the data in /ds/ using the 'Field' /x/ for the values on the /x/-axis
-- and with one line on the plot for each 'Field' in /ys/. See also 'field'.
plotRows :: [a] -> Field a -> [Field a] -> Doc
plotRows d x ys = plotRows' d x ys []

-- | Same as 'plotRows', but takes a final argument which is merged with the
-- configuration object supplied to <http://c3js.org/ C3.js>. This allows the
-- caller to customize the plot.
--
-- @
--  -- Plot x² vs x with the points hidden.
--  plotRows' [1..10] (field "x" id) [field "x²" $ \\x -> x*x] $
--    ["point" //: ["show" //: False]]
-- @
--
-- See <http://c3js.org/reference.html> for the many properties that can be used
-- here.
plotRows' :: [a] -> Field a -> [Field a] -> [Prop] -> Doc
plotRows' d x ys = plotCols' (g x) (map g ys)
  where
    g (Field h f) = Column h (map f d)

-- | Represent a column in the plot. See 'col'.
data Column = Column
  { _colHeader :: String
  , _data      :: [Obj]
  }

-- | > col h d
--
-- Constructs a column with the header /h/ (used in the legend or on the
-- x-axis) and the data in /d/.
col :: String -> [Double] -> Column
col = col'

-- | Polymorphic version of 'col'. This allows, for instance, 'Int' and
-- 'String' values to be used in plots. It is up to the caller to ensure that
-- the values make sense to <http://c3js.org/ C3.js>.
col' :: ToObj a => String -> [a] -> Column
col' h d = Column h (map toObj d)

plotCols :: Column -> [Column] -> Doc
plotCols x ys = plotCols' x ys []

-- | Same as 'plotCols', but takes a final argument which is merged with the
-- configuration object supplied to <http://c3js.org/ C3.js>. This allows the
-- caller to customize the plot.
--
-- @
--  -- Plot x² vs x with the points hidden.
--  plotCols [1..10] (col "x" [1..10]) [col "x²" [x*x for x in [1..10]]] $
--    ["point" //: ["show" //: False]]
-- @
--
-- See <http://c3js.org/reference.html> for the many properties that can be used
-- here.
plotCols' :: Column -> [Column] -> [Prop] -> Doc
plotCols' x@(Column xheader xdata) ys options =
  let
    names = map toObj (xheader : map _colHeader ys)
    values = transpose $ map _data (x:ys)
    obj =
      [ "data" /:
        [ "rows" /: (names : values)
        , "x" /: xheader
        ]
      ]
  in
    assert (all (== length xdata) $ map (length . _data) ys) $
      plotRaw $ obj ++ options


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
