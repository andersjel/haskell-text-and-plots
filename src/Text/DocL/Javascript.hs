module Text.DocL.Javascript where

import Data.Char            (isControl, ord)
import Data.List            (intercalate)
import Text.Printf          (printf)

import qualified Data.Map.Strict      as Map

data Prop = Prop String Obj
data Obj = Obj (Map.Map String Obj) | List [Obj] | Scalar String

class ToObj a where
  toObj :: a -> Obj
  toObjList :: [a] -> Obj
  toObjList = List . map toObj

instance ToObj Prop where
  toObj x = toObj [x]
  toObjList = Obj . Map.fromListWith merge . map (\(Prop k v) -> (k, v))

instance ToObj a => ToObj [a] where
  toObj = toObjList

(/:) :: ToObj a => String -> a -> Prop
k /: v = Prop k $ toObj v

merge :: Obj -> Obj -> Obj
merge (Obj x) (Obj y) = Obj $ Map.unionWith merge x y
merge _ x = x

javascript :: String -> Obj
javascript = Scalar

instance ToObj Obj where
  toObj = id

instance ToObj Double where
  toObj = Scalar . show
instance ToObj Int where
  toObj = Scalar . show
instance ToObj Integer where
  toObj = Scalar . show

instance ToObj () where
  toObj () = Scalar "null"

instance ToObj Bool where
  toObj x = Scalar $ if x then "true" else "false"

instance ToObj Char where
  toObj c = toObj [c]
  toObjList xs = Scalar $ "\"" ++ concatMap esc xs ++ "\""
    where
      mustEscape = "\"\\<\n\r\x2028\x2029"
      esc x =
        if isControl x || elem x mustEscape
          then printf "\\u%04x" $ ord x
          else [x]

encode :: Obj -> String
encode (Scalar x) = x
encode (List xs) = "[" ++ intercalate "," (map encode xs) ++ "]"
encode (Obj m) = "{" ++ intercalate "," (map f $ Map.toList m) ++ "}"
  where f (k, v) = encode (toObj k) ++ ":" ++ encode v
