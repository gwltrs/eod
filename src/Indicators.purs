module Indicators where

import Prelude

import Class.RandomAccess (class RandomAccess, rLen, rAt)
import Data.Foldable (sum)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Number (infinity)
import Data.Slice (Slice, sat, slen)
import Forceable (frc)
import Type.Indicator (Indicator, last)
import Type.Day (Day(..), avg, fourPrice)
import Data.Newtype (unwrap)
import Utils (undefined, slices)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (Tuple3, tuple3, (/\))
import Data.Unfoldable (unfoldr)
import Partial.Unsafe (unsafeCrashWith)
import Data.Foldable (foldr, fold)
import Data.Ord (abs, between)
import Debug (spy)

at :: Int -> Indicator Day
at i = 
  let i' = max 0 i
  in frc <$> last (1 + i')

atr :: Slice Day -> Number
atr days =
  if rLen days < 2 then 
    unsafeCrashWith "average true range calculation requires at least 2 days"
  else
    let 
      trueRange :: Day -> Day -> Number
      trueRange (Day a) (Day b) = [b.high - b.low, b.high - a.close, b.low - a.close]
        # map abs
        # foldr max 0.0
    in 
      slices 2 days
        <#> (\a -> trueRange (rAt 0 a) (rAt 1 a))
        # foldr (+) 0.0
        # (_ / (toNumber $ rLen days))
  
bullishReverse :: forall r o. RandomAccess r => Ord o => r o -> Boolean
bullishReverse r = a >= b && b < c
  where 
    a = rAt ((rLen r) - 3) r
    b = rAt ((rLen r) - 2) r
    c = rAt ((rLen r) - 1) r

convex :: forall r. RandomAccess r => r Number -> Int
convex ns =
  let 
    l = rLen ns
    d' i = (rAt i ns) - (rAt (i + 1) ns)
    f0 i d = if i < 0 || ((d' i) <= d) then max 0 (rLen ns - i - 1) else f0 (i - 1) (d' i)
  in
    let x = f0 (l - 3) (d' (l - 2))
    in if x > 2 then x else 0

fibChunks :: forall r s. RandomAccess r => Semigroup s => r s -> Array s
fibChunks r = 
  let
    calcFibs fib0 fib1 fibSum = 
      if rLen r == 0 then 
        tuple3 0 0 0
      else 
        if (fibSum + fib0 + fib1) > rLen r 
        then tuple3 fib0 fib1 fibSum 
        else calcFibs fib1 (fib0 + fib1) (fibSum + fib0 + fib1)
    fib0 /\ fib1 /\ fibSum /\ unit = calcFibs 0 1 1
    emit (_ /\ 0 /\ _ /\ _) = Nothing
    emit (fib0 /\ fib1 /\ index /\ unit) = Just (Tuple (Tuple index (index + fib1)) (tuple3 (fib1 - fib0) fib0 (index + fib1)))
    ranges = unfoldr emit (tuple3 fib0 fib1 (rLen r - fibSum))
    mapRange (Tuple start end) = if start == (end - 1) then rAt start r else rAt start r <> mapRange (Tuple (start + 1) end)
  in
    mapRange <$> ranges

isInvertedCricket :: Day -> Boolean
isInvertedCricket (Day d) = 
  let
    center = (d.high + d.low) / 2.0
    magnitude = d.high - d.low
    fivePercent = magnitude * 0.05
    bodyTop = max d.open d.close
    bodyBottom = min d.open d.close
    topIsHalfway = between
      (center - fivePercent) 
      (center + fivePercent) 
      bodyTop
    bottomIsLow = between
      d.low
      (d.low + fivePercent)
      bodyBottom
  in
    topIsHalfway

isGreen :: Day -> Boolean
isGreen (Day d) = d.close > d.open

isUp :: Day -> Day -> Boolean
isUp (Day a) (Day b) = b.close > a.close