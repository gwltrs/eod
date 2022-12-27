module Utils where

import Prelude

import Data.CodePoint.Unicode (isAlpha)
import Data.Date (Date, Month(..), exactDate)
import Data.Enum (toEnum)
import Data.Foldable (foldr, intercalate)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Slice (Slice, sempty, slen, slice, stail, stake)
import Data.String (length, toCodePointArray)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (unfoldr)

allTrue :: Array Boolean -> Boolean
allTrue = foldr (&&) true

isAlphaStr :: String -> Boolean
isAlphaStr = toCodePointArray >>> (_ <#> isAlpha) >>> allTrue

toJSONArray :: forall a. (a -> String) -> Array a -> String
toJSONArray f a = 
  let inner = a <#> f # intercalate ","
  in "[" <> inner <> "]"

slices :: forall a. Int -> Array a -> Array (Slice a)
slices n arr = 
  let f s = if slen s >= n && n > 0 then Just (Tuple (stake n s) (fromMaybe sempty (stail s))) else Nothing
  in unfoldr f (slice arr)