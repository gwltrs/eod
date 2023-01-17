module Indicators where

import Prelude

import Class.RandomAccess (rAt)
import Data.Foldable (sum)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Number (infinity)
import Data.Slice (Slice, sat, slen)
import Forceable (frc)
import Type.Indicator (Indicator, indicator)
import Type.LiveDay (LiveDay, _c, avg)

closes :: Indicator (Slice Number)
closes = indicator 0 (map _c)

day :: Indicator LiveDay
day = indicator 1 frc

lastN :: forall a. Int -> Indicator a -> Indicator a
lastN n = (indicator n identity *> _)

sma :: Indicator (Slice Number -> Number)
sma = indicator 1 (const (\ns -> sum ns / (toNumber $ slen ns)))

convex :: (LiveDay -> Number) -> Indicator Int
convex fd =
  let 
    d' n s = (fd $ rAt n s) - (fd $ rAt (n + 1) s)
    f0 s n d = if n < 0 || ((d' n s) <= d) then max 0 (10 - n - 1) else f0 s (n - 1) (d' n s)
    f1 s = let x = f0 s 7 (d' 8 s) in if x > 2 then x else 0
  in indicator 10 f1

-- convexness :: Indicator (Maybe Int)
-- convexness = indicator 10