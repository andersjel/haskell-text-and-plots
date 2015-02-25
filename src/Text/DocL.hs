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
  Column, col, col',
  plot, plot', rawPlot,
  -- * Utilities
  module Text.DocL.Javascript,
  linspace
  ) where

import Data.Foldable
import Data.HashMap.Strict           (insert, unionWith)
import Data.Monoid
import Data.Sequence                 (Seq, singleton)
import Data.String                   (IsString (..))
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Blaze.Html5              ((!))
import Text.DocL.Javascript

import qualified Text.Markdown               as Markdown
import qualified Data.Aeson                  as Aeson
import qualified System.IO                   as IO
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Data.ByteString.Lazy        as B

data Node = Chart Obj | Html H.Html

-- | Base type representing a document. This type allows composition via the
-- 'Monoid' instance.
newtype Doc = Doc (Seq Node) deriving Monoid

-- | Represent a column of a dataset where each row has type /a/. See 'col' for
-- details.
data Column a = Column
  { _header  :: String
  , _extract :: a -> Obj
  }

-- | Polymorphic version of 'col'. This allows, for instance, 'Int' and 'String'
-- values to be used in plots. It is up to the caller to ensure that the values
-- make sense to <http://c3js.org/ C3.js>.
col' :: ToJSON b => String -> (a -> b) -> Column a
col' h f = Column h (toJSON . f)

-- | Data sets are supplied to 'plot' as a collection of elements of type /a/.
-- This function sets up a 'Column', which knows how to extract one 'Double'
-- value from each value of type /a/ in the collection.
--
-- The first argument to 'col' is the header of the column, which is used in the
-- legend of the plot.
--
-- The simplest plot that can be created consist of one column for the /x/
-- values and one column for the /y/ values.
--
-- @
--  -- Plot x² vs x.
--  plot [1..10] (col "x" id) [col "x²" $ \\x -> x*x]
-- @
col :: String -> (a -> Double) -> Column a
col = col'

-- | > plot ds x ys
--
-- Plots the data in /ds/ using the column /x/ for the values on the /x/-axis
-- and with one line on the plot for each column in /ys/. See also 'col'.
plot :: Foldable f => f a -> Column a -> [Column a] -> Doc
plot d x ys = plot' d x ys []

-- | Same as 'plot', but takes a final argument which is merged with the
-- configuration object supplied to <http://c3js.org/ C3.js>. This allows the
-- caller to customize the plot.
--
-- @
--  -- Plot x² vs x with the points hidden.
--  plot' [1..10] (col "x" id) [col "x²" $ \\x -> x*x] $
--    ["point" /: ["show" /: False]]
-- @
--
-- See <http://c3js.org/reference.html> for the many properties that can be used
-- here.
plot' :: Foldable f => f a -> Column a -> [Column a] -> [Prop] -> Doc
plot' d x ys options = rawPlot $ merge obj options
  where
    obj = object
      [ "data" .= object
        [ "rows" .= (toJSON (_header x : map _header ys) : map f (toList d))
        , "x" .= _header x
        ]
      ]
    f p = toJSON $ map (`_extract` p) (x:ys)

-- | This function sends the supplied json object directly to
-- <http://c3js.org/ C3.js>. This is the do-it-yourself option which exposes all
-- of <http://c3js.org/ C3.js>. The object receives a
-- <http://c3js.org/reference.html#bindto "bindto"> property,
-- targeting a @\<div\>@ tag placed appropriately.
rawPlot :: Aeson.ToJSON a => a -> Doc
rawPlot = Doc . singleton . Chart . Aeson.toJSON

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
    f (i, (Chart v)) = divTag <> scriptTag
      where
        name = "plot" <> show i
        divTag = H.div ! A.id (fromString name) $ mempty
        (Aeson.Object obj) = v
        obj' = insert "bindto" (fromString $ "#" <> name) obj
        json = H.unsafeLazyByteString $ Aeson.encode obj'
        scriptTag = H.script ! A.type_ "text/javascript" $
          H.preEscapedToMarkup $ "c3.generate(" <> json <> ");"

-- | If the file exists, it will be overwritten.
renderToFile :: FilePath -> Doc -> IO ()
renderToFile p = B.writeFile p . render

-- | /'linspace' x y n/ generates /n/ evenly spaced values from /x/ to /y/.
linspace :: Fractional a => a -> a -> Int -> [a]
linspace x y n = [x + (fromIntegral i)*d | i <- [0 .. n - 1]]
  where d = (y - x) / (fromIntegral n - 1)
