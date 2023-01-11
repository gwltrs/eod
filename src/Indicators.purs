module Indicators where

import Prelude

import Class.RandomAccess (rAt)
import Data.Foldable (sum)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Number (infinity)
import Data.Slice (Slice, sat)
import Forceable (frc)
import Type.Indicator (Indicator, indicator)
import Type.LiveDay (LiveDay, avg)

day :: Indicator LiveDay
day = indicator 1 frc

sma :: Int -> Indicator Number
sma n = indicator n (\d -> d <#> _.close # sum # (_ / toNumber n))

convex :: Indicator Int
convex =
  let 
    d' n s = (avg $ rAt n s) - (avg $ rAt (n + 1) s)
    f0 s n d = if n < 0 || ((d' n s) <= d) then max 0 (10 - n - 1) else f0 s (n - 1) (d' n s)
    f1 s = let x = f0 s 7 (d' 8 s) in if x > 2 then x else 0
  in indicator 10 f1

-- convexness :: Indicator (Maybe Int)
-- convexness = indicator 10