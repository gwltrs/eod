module Type.BacktestResult where

import Prelude
import Utils (undefined)
import Data.Foldable (class Foldable)
import Type.Alias (Ticker)
import Type.YMD (YMD)

newtype BacktestResult = BacktestResult { ticker :: Ticker, date :: YMD, rMultiple :: Number }

expectancy :: forall f. Foldable f => f BacktestResult -> Number
expectancy = undefined