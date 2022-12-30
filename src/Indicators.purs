module Type.Indicator.Indicators where

import Prelude

import Data.Foldable (sum)
import Data.Slice (sat)
import Forceable (frc)
import Type.Indicator (Indicator(..))

sma :: Int -> Indicator Number
sma n = Indicator { n: n, f: (\days -> days <#> _.close # sum) }

price :: Indicator Number
price = Indicator { n: 1, f: (\days -> (frc $ sat days 0).close) }