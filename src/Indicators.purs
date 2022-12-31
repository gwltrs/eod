module Indicators where

import Prelude

import Data.Foldable (sum)
import Data.Int (toNumber)
import Data.Slice (sat)
import Forceable (frc)
import Type.Indicator (Indicator, indicator)

lastPrice :: Indicator Number
lastPrice = indicator 1 (frc >>> _.close)

sma :: Int -> Indicator Number
sma n = indicator n (\d -> d <#> _.close # sum # (_ / toNumber n))