module Indicators where

import Prelude

import Data.Slice (Slice)
import Type.LiveDay (LiveDay)

sma :: Slice LiveDay -> Boolean
sma _ = false