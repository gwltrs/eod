module Indicators where

import Prelude

import Data.Foldable (sum)
import Data.Int (toNumber)
import Data.Slice (sat)
import Forceable (frc)
import Type.Indicator (Indicator, indicator)

day :: Indicator { open :: Number, high :: Number, low :: Number, close :: Number, volume :: Number }
day = indicator 1 frc

sma :: Int -> Indicator Number
sma n = indicator n (\d -> d <#> _.close # sum # (_ / toNumber n))