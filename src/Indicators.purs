module Indicators
  ( at
  , convex
  )
  where

import Prelude

import Class.RandomAccess (class RandomAccess, rLen, rAt)
import Data.Foldable (sum)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Number (infinity)
import Data.Slice (Slice, sat, slen)
import Forceable (frc)
import Type.Indicator (Indicator, last)
import Type.Day (Day, avg, fourPrice)
import Data.Newtype (unwrap)
import Utils (undefined)

-- closes :: Indicator (Slice Number)
-- closes = indicator 0 (map $ unwrap >>> _.close)

-- day :: Indicator Day
-- day = indicator 1 frc

-- lastN :: forall a. Int -> Indicator a -> Indicator a
-- lastN n = (indicator n identity *> _)

-- sma :: Indicator (Slice Number -> Number)
-- sma = indicator 1 (const (\ns -> sum ns / (toNumber $ slen ns)))

at :: Int -> Indicator Day
at i = 
  let i' = max 0 i
  in frc <$> last (1 + i')

convex :: forall r. RandomAccess r => r Number -> Int
convex ns =
  let 
    l = rLen ns
    d' i = (rAt i ns) - (rAt (i + 1) ns)
    f0 i d = if i < 0 || ((d' i) <= d) then max 0 (rLen ns - i - 1) else f0 (i - 1) (d' i)
  in
    let x = f0 (l - 3) (d' (l - 2))
    in if x > 2 then x else 0