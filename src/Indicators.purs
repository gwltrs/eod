module Indicators where

import Prelude

import Class.RandomAccess (class RandomAccess, rAt)
import Data.Foldable (sum)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Number (infinity)
import Data.Slice (Slice, sat, slen)
import Forceable (frc)
import Type.Indicator (Indicator)
import Type.Day (Day, avg)
import Data.Newtype (unwrap)

-- closes :: Indicator (Slice Number)
-- closes = indicator 0 (map $ unwrap >>> _.close)

-- day :: Indicator Day
-- day = indicator 1 frc

-- lastN :: forall a. Int -> Indicator a -> Indicator a
-- lastN n = (indicator n identity *> _)

-- sma :: Indicator (Slice Number -> Number)
-- sma = indicator 1 (const (\ns -> sum ns / (toNumber $ slen ns)))

convex :: forall r. RandomAccess r => r Number -> Int
convex ns = -1
  --let 
  --  d' n s = (ns $ rAt n s) - (ns $ rAt (n + 1) s)
  --  f0 s n d = if n < 0 || ((d' n s) <= d) then max 0 (10 - n - 1) else f0 s (n - 1) (d' n s)
  --  f1 s = let x = f0 s 7 (d' 8 s) in if x > 2 then x else 0
  --in -1 -- indicator 10 f1

-- convexness :: Indicator (Maybe Int)
-- convexness = indicator 10