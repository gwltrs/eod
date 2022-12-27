module Indicators where

import Prelude

import Data.Number (abs)
import Data.Slice (Slice, sat, slast, slen)
import Forceable (frc)
import Type.LiveDay (LiveDay, liveDay)

sma :: Slice LiveDay -> Boolean
sma _ = false

longFullness :: LiveDay -> Number
longFullness d = 
  if d.close > d.open
  then (d.close - d.open) / (d.high - d.low)
  else 0.0