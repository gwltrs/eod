module Utils where

import Prelude

import Data.CodePoint.Unicode (isAlpha)
import Data.Date (Date, Month(..), exactDate)
import Data.Enum (toEnum)
import Data.Foldable (foldr, intercalate)
import Data.Maybe (Maybe(..))
import Data.String (length, toCodePointArray)

allTrue :: Array Boolean -> Boolean
allTrue = foldr (&&) true

isAlphaStr :: String -> Boolean
isAlphaStr = toCodePointArray >>> (_ <#> isAlpha) >>> allTrue

toJSONArray :: forall a. (a -> String) -> Array a -> String
toJSONArray f a = 
  let inner = a <#> f # intercalate ","
  in "[" <> inner <> "]"