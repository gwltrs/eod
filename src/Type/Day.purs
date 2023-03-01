module Type.Day
  ( Day(..)
  , avg
  , close
  , day
  , dayFromJSON
  , fourPrice
  , high
  , low
  , open
  )
  where

import Prelude

import Data.Argonaut.Core (Json, toArray, toNumber, toObject, toString)
import Data.Argonaut.Parser (jsonParser)
import Data.Maybe (Maybe(..))
import Foreign.Object (lookup)
import Railroad (rightToMaybe)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (choose)
import Data.Newtype (class Newtype)

newtype Day = Day { open :: Number, high :: Number, low :: Number, close :: Number, volume :: Number }

day :: Number -> Number -> Number -> Number -> Number -> Day
day o h l c v = Day { open: o, high: h, low: l, close: c, volume: v }

dayFromJSON :: String -> Maybe Day
dayFromJSON json = do
  obj <- jsonParser json # rightToMaybe >>= toObject
  o <- lookup "open" obj >>= toNumber
  h <- lookup "high" obj >>= toNumber
  l <- lookup "low" obj >>= toNumber
  c <- lookup "close" obj >>= toNumber
  v <- lookup "volume" obj >>= toNumber
  pure $ day o h l c v

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
fourPrice n = Day { open: n, high: n, low: n, close: n, volume: n }

avg :: Day -> Number
avg (Day d) = (d.open + d.high + d.low + d.close) / 4.0

derive instance newtypeDay :: Newtype Day _

derive instance eqDay :: Eq Day

instance showDay :: Show Day where
  show (Day d) = "Day " <> show d

instance arbitraryDay :: Arbitrary Day where
  arbitrary = do
    h <- choose 0.0 1000.0
    l <- choose 0.0 h
    o <- choose l h
    c <- choose l h
    v <- choose 0.0 1_000_000_000.0
    pure $ day o h l c v

instance semigroupDay :: Semigroup Day where
  append a b = day 
    (open a) 
    (max (high a) (high b)) 
    (min (low a) (low b)) 
    (close b) 
    (volume a + volume b)