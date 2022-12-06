module Utils where

import Prelude

import Data.CodePoint.Unicode (isAlpha)
import Data.Date (Date, Month(..), exactDate)
import Data.Enum (toEnum)
import Data.Foldable (foldr)
import Data.Maybe (Maybe(..))
import Data.String (length, toCodePointArray)

allTrue :: Array Boolean -> Boolean
allTrue = foldr (&&) true

isAlphaStr :: String -> Boolean
isAlphaStr = toCodePointArray >>> (_ <#> isAlpha) >>> allTrue

date :: Int -> Int -> Int -> Maybe Date
date y m d = do
  year <- toEnum y
  month <- toEnum m
  day <- toEnum d
  exactDate year month day