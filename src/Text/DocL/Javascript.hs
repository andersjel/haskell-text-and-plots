module Text.DocL.Javascript (
  Prop, Obj,
  ToObj (..), (/:),
  javascript,
  encode
  ) where

import Data.Char      (isControl, ord)
import Data.Foldable  hiding (elem)
import Data.List      (intersperse)
import Data.Monoid
import Data.String    (IsString (..))
import Data.Text.Lazy (Text)
import Text.Printf    (printf)

import qualified Data.HashMap.Strict    as Map
import qualified Data.Text.Lazy         as T
import qualified Data.Text.Lazy.Builder as B

data Prop = Prop Text Obj
data Obj = Obj (Map.HashMap Text Obj) | List [Obj] | Scalar Text

class ToObj a where
  toObj :: a -> Obj
  toObjList :: [a] -> Obj
  toObjList = List . map toObj

-- | A ['Prop'] becomes a javascript object. See the '/:'-operator.
instance ToObj Prop where
  toObj x = toObj [x]
  toObjList = Obj . Map.fromListWith merge . map (\(Prop k v) -> (k, v))

-- | Becomes a javascript array.
instance ToObj a => ToObj [a] where
  toObj = toObjList

(/:) :: ToObj a => String -> a -> Prop
k /: v = Prop (fromString k) $ toObj v

merge :: Obj -> Obj -> Obj
merge (Obj x) (Obj y) = Obj $ Map.unionWith merge x y
merge _ x = x

javascript :: String -> Obj
javascript = Scalar . fromString

-- | 'ToObj' is the identity for an 'Obj' value.
instance ToObj Obj where
  toObj = id

instance ToObj Double where
  -- This trivial implementation even handles NaN and Infinity correctly.
  toObj = Scalar . fromString . show
instance ToObj Int where
  toObj = Scalar . fromString . show
instance ToObj Integer where
  toObj = Scalar . fromString . show

-- | Becomes a javascript /null/ object
instance ToObj () where
  toObj () = Scalar "null"

instance ToObj Bool where
  toObj x = Scalar $ if x then "true" else "false"

-- | A 'String' becomes a javascript string literal.
instance ToObj Char where
  toObj c = toObj [c]
  toObjList xs = Scalar . B.toLazyText $ "\"" <> foldMap esc xs <> "\""
    where
      mustEscape = "\"\\>\n\r\x2028\x2029"
      esc x =
        if isControl x || elem x mustEscape
          then fromString . printf "\\u%04x" $ ord x
          else B.singleton x

-- | Becomes a javascript string literal.
instance ToObj Text where
  toObj = toObj . T.unpack

encode :: Obj -> Text
encode = B.toLazyText . encode'

encode' :: Obj -> B.Builder
encode' obj = case obj of
    Scalar x -> B.fromLazyText x
    List xs  -> "[" <> commas (map encode' xs) <> "]"
    Obj m    -> "{" <> commas (map f $ Map.toList m) <> "}"
  where
    commas = fold . intersperse ","
    f (k, v) = encode' (toObj k) <> ":" <> encode' v
