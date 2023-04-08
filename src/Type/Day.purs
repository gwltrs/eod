module Type.Day
  ( Day(..)
  , avg
  , close
  , day
  , fourPrice
  , high
  , low
  , open
  , ticker
  , date
  , fromBulkDay
  , fromEODDay
  , fromLiveDay
  )
  where

import Prelude

import Data.Argonaut.Core (Json, toArray, toNumber, toObject, toString)
import Data.Argonaut.Parser (jsonParser)
import Data.Maybe (Maybe(..))
import Foreign.Object (lookup)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (choose)
import Data.Newtype (class Newtype)
import Type.YMD (YMD(..), ymd)
import Forceable (frc)
import Partial.Unsafe (unsafeCrashWith)
import Type.Alias (Ticker)
import Type.JSON.EODDay (EODDay(..))
import Type.JSON.BulkDay (BulkDay(..))
import Type.JSON.LiveDay (LiveDay(..))
import Utils (undefined)

newtype Day = Day 
  { ticker :: Ticker
  , date :: YMD
  , open :: Number
  , high :: Number
  , low :: Number
  , close :: Number
  , volume :: Number
  }

day :: Ticker -> YMD -> Number -> Number -> Number -> Number -> Number -> Day
day t d o h l c v = Day { ticker: t, date: d, open: o, high: h, low: l, close: c, volume: v }

fromEODDay :: Ticker -> EODDay -> Day
fromEODDay t ed = day t ed.date ed.open ed.high ed.low ed.close ed.volume

fromBulkDay :: BulkDay -> Day
fromBulkDay bd = day bd.code bd.date bd.open bd.high bd.low bd.close bd.volume

fromLiveDay :: Ticker -> YMD -> LiveDay -> Day
fromLiveDay t y ld = day t y ld.open ld.high ld.low ld.close ld.volume

ticker :: Day -> Ticker
ticker (Day d) = d.ticker

date :: Day -> YMD
date (Day d) = d.date

open :: Day -> Number
open (Day d) = d.open

high :: Day -> Number
high (Day d) = d.high

low :: Day -> Number
low (Day d) = d.low

close :: Day -> Number
close (Day d) = d.close

volume :: Day -> Number
volume (Day d) = d.volume

fourPrice :: Number -> Day
fourPrice n = Day { ticker: "", date: frc $ ymd 1900 1 1, open: n, high: n, low: n, close: n, volume: n }

avg :: Day -> Number
avg (Day d) = (d.open + d.high + d.low + d.close) / 4.0

derive instance newtypeDay :: Newtype Day _

derive instance eqDay :: Eq Day

instance showDay :: Show Day where
  show (Day d) = "Day " <> show d

--instance arbitraryDay :: Arbitrary Day where
--  arbitrary = do
--    d <- pure $ frc $ ymd 1900 1 1
--    h <- choose 0.0 1000.0
--    l <- choose 0.0 h
--    o <- choose l h
--    c <- choose l h
--    v <- choose 0.0 1_000_000_000.0
--    pure $ day d o h l c v

instance semigroupDay :: Semigroup Day where
  append a b = 
    if (ticker a) /= (ticker b) 
    then unsafeCrashWith "Semigroup Day: don't append days with different tickers"
    else
      day
        (ticker a)
        (date a) 
        (open a) 
        (max (high a) (high b)) 
        (min (low a) (low b)) 
        (close b) 
        (volume a + volume b)