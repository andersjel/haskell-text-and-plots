module Text.DocL.Javascript (
-- * Rationale
-- | This is basically a JSON encoding library, however it goes beyond JSON by
-- allowing raw javascript to be used as a value. This allows, for instance,
-- function calls and anonymous functions to be placed in the data when used in
-- a javascript context. This is needed to expose some options in
-- <http://c3js.org/ C3.js>, e.g. customizing tick formatting.

-- * The 'Obj' type
  Obj,
  ToObj (..),
-- * Constructing javascript types
  Prop, (/:), javascript,
-- * Encoding
  encode,
-- * Miscellanea
  merge
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

-- | A key\/value pair created with the '/:'-operator.
--
-- A list of type ['Prop'] can be converted to an object using 'toObj'. If the
-- list contains duplicate keys, then 'merge' is used to combine the values.
--
-- >>> toObj ["legs" /: 4, "ears" /: "floppy"]
-- {"ears":"floppy","legs":4}
data Prop = Prop Text Obj

-- | Represents a javascript object or scalar value.
data Obj = Obj (Map.HashMap Text Obj) | List [Obj] | Scalar Text

instance IsString Obj where
  fromString = toObj

-- | /'show' x/ returns /x/ encoded.
instance Show Obj where
  show = T.unpack . encode

-- | Class of types that can be converted to an 'Obj'.
class ToObj a where
  -- | Converts to an 'Obj'.
  toObj :: a -> Obj

  -- | Only for writing instances. This function serves the same role as
  -- 'showList'
  toObjList :: [a] -> Obj
  toObjList = List . map toObj

-- | A ['Prop'] becomes a javascript object.
instance ToObj Prop where
  toObj x = toObj [x]
  toObjList = Obj . Map.fromListWith merge . map (\(Prop k v) -> (k, v))

-- | Becomes a javascript array.
instance ToObj a => ToObj [a] where
  toObj = toObjList

-- | Constructs a 'Prop'.
--
-- >>> toObj ["tail" /: True, "nose" /: "wet"]
-- {"nose":"wet","tail":true}
(/:) :: ToObj a => String -> a -> Prop
k /: v = Prop (fromString k) $ toObj v

-- | /'merge' x y/ returns /y/ unless both /x/ and /y/ are javascript objects,
-- in which case the properties of the two are merged using 'merge' recursively
-- for duplicate keys.
merge :: Obj -> Obj -> Obj
merge (Obj x) (Obj y) = Obj $ Map.unionWith merge x y
merge _ x = x

-- | Raw unescaped javascript.
--
-- >>> toObj ["speak" /: javascript "function(){alert('Woof!');}"]
-- {"speak":function(){alert('Woof!');}}
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
      mustEscape = "\"\\>\n\r\x2028\x2029" :: String
      esc x =
        if isControl x || elem x mustEscape
          then fromString . printf "\\u%04x" $ ord x
          else B.singleton x

-- | Becomes a javascript string literal.
instance ToObj Text where
  toObj = toObj . T.unpack

-- | Encode an 'Obj' to a lazy 'Text'.
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
