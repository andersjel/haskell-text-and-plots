module Text.DocL
  ( Doc
  , text, header, markdown, html
  , Column, col
  , plot, plot', rawPlot
  , render, renderToFile
  ) where

import Data.Aeson                    (ToJSON, object, toJSON, (.=))
import Data.Foldable
import Data.HashMap.Strict           (insert, unionWith)
import Data.Monoid
import Data.Sequence                 (Seq, singleton)
import Data.String                   (IsString (..))
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Blaze.Html5              ((!))

import qualified Text.Markdown               as Markdown
import qualified Data.Aeson                  as Aeson
import qualified System.IO                   as IO
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Data.ByteString.Lazy        as B

data Node = Chart Aeson.Value | Html H.Html
newtype Doc = Doc (Seq Node) deriving Monoid

data Column a = Column
  { _header  :: String
  , _extract :: a -> Double
  }

merge :: Aeson.Value -> Aeson.Value -> Aeson.Value
merge (Aeson.Object x) (Aeson.Object y) = Aeson.Object $ unionWith merge x y
merge _ x = x

col :: String -> (a -> Double) -> Column a
col = Column

plot :: Foldable f => f a -> Column a -> [Column a] -> Doc
plot d x ys = plot' d x ys (Aeson.Object mempty)

plot' :: Foldable f => f a -> Column a -> [Column a] -> Aeson.Value -> Doc
plot' d x ys options = rawPlot $ merge obj options
  where
    obj = object
      [ "data" .= object
        [ "rows" .= (toJSON (_header x : map _header ys) : map f (toList d))
        , "x" .= _header x
        ]
      ]
    f p = toJSON $ map (`_extract` p) (x:ys)

rawPlot :: Aeson.ToJSON a => a -> Doc
rawPlot = Doc . singleton . Chart . Aeson.toJSON

html :: H.Html -> Doc
html = Doc . singleton . Html

text :: String -> Doc
text = html . H.p . H.toMarkup

header :: String -> Doc
header = html . H.h1 . H.toMarkup

markdown :: String -> Doc
markdown = html . Markdown.markdown Markdown.def . fromString

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

renderToFile :: FilePath -> Doc -> IO ()
renderToFile p = B.writeFile p . render
