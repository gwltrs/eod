module Type.Indicator.Indicators where

import Prelude

import Data.Foldable (sum)
import Data.Slice (sat)
import Forceable (frc)
import Type.Indicator (Indicator(..))

sma :: Int -> Indicator Number
sma n = Indicator { minNumDays: n, indicate: (\days -> days <#> _.close # sum) }

lastPrice :: Indicator Number
lastPrice = Indicator { minNumDays: 1, indicate: (\days -> (frc $ sat days 0).close) }