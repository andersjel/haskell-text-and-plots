module Text.DocL.Javascript where

data Prop = Prop String Obj
data Obj = Obj (Map String Obj) | List [Obj] | Scalar String

class ToObj a where
  toObj :: a -> Obj
  toObjList :: [a] -> Obj
  toObjList = List . map toObj

instance ToObj Prop where
  toObj x = toObj [x]
  toObjList = Obj . fromList . map (\(Prop k v) -> (k, v))

instance ToObj a => ToObj [a] where
  toObj = toObjList

(/:) :: ToObj a => String -> a -> Prop a
k /: v = Prop k $ toObj v

merge :: Obj -> Obj -> Obj
merge (Obj x) (Obj y) = Obj $ unionWith merge x y
merge _ x = x

javascript :: String -> Obj
javascript = undefined
