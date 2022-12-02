module Utils where

import Prelude

import Data.Foldable (foldr)
import Data.String (length, toCodePointArray)
import Data.CodePoint.Unicode (isAlpha)

allTrue :: Array Boolean -> Boolean
allTrue = foldr (&&) true

isAlphaStr :: String -> Boolean
isAlphaStr = toCodePointArray >>> (_ <#> isAlpha) >>> allTrue