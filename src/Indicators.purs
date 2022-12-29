module Indicators where

import Prelude

import Class.RandomAccess (class RandomAccess, rAt, rLen)
import Data.Foldable (foldr, maximum)
import Data.Int (toNumber)
import Data.Number (abs)
import Data.Slice (Slice, sat, slast, slen)
import Forceable (frc)
import Type.LiveDay (LiveDay, liveDay)
import Utils (slices)

longFullness :: LiveDay -> Number
longFullness d = 
  if d.close > d.open
  then (d.close - d.open) / (d.high - d.low)
  else 0.0

atr :: forall r. RandomAccess r => r LiveDay -> Number
atr r = 
  let 
    tr a b = frc $ maximum $ abs <$> [b.high - b.low, b.high - a.close, b.low - a.close]
    sumTR i0 i1 x = if i0 < i1 then tr (rAt i0 r) (rAt (i0 + 1) r) + sumTR (i0 + 1) i1 x else x
    summedTR = sumTR 0 (rLen r - 1) 0.0
    count = toNumber (rLen r - 1)
  in    
    summedTR / count