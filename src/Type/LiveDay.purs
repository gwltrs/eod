module Type.LiveDay
  ( LiveDay(..)
  , _c
  , _h
  , _l
  , _o
  , _v
  , avg
  , liveDay
  , liveDayFromJSON
  , noMove
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

data LiveDay = LiveDay { open :: Number, high :: Number, low :: Number, close :: Number, volume :: Number }

liveDay :: Number -> Number -> Number -> Number -> Number -> LiveDay
liveDay o h l c v = LiveDay { open: o, high: h, low: l, close: c, volume: v }

liveDayFromJSON :: String -> Maybe LiveDay
liveDayFromJSON json = do
  obj <- jsonParser json # rightToMaybe >>= toObject
  o <- lookup "open" obj >>= toNumber
  h <- lookup "high" obj >>= toNumber
  l <- lookup "low" obj >>= toNumber
  c <- lookup "close" obj >>= toNumber
  v <- lookup "volume" obj >>= toNumber
  pure $ liveDay o h l c v

_o :: LiveDay -> Number
_o (LiveDay d) = d.open

_h :: LiveDay -> Number
_h (LiveDay d) = d.high

_l :: LiveDay -> Number
_l (LiveDay d) = d.low

_c :: LiveDay -> Number
_c (LiveDay d) = d.close

_v :: LiveDay -> Number
_v (LiveDay d) = d.volume

noMove :: Number -> LiveDay
noMove n = LiveDay { open: n, high: n, low: n, close: n, volume: n }

avg :: LiveDay -> Number
avg (LiveDay d) = (d.open + d.high + d.low + d.close) / 4.0

derive instance eqLiveDay :: Eq LiveDay
instance showLiveDay :: Show LiveDay where
  show (LiveDay d) = "LiveDay " <> show d

instance arbitraryLiveDay :: Arbitrary LiveDay where
  arbitrary = do
    h <- choose 0.0 1000.0
    l <- choose 0.0 h
    o <- choose l h
    c <- choose l h
    v <- choose 0.0 1_000_000_000.0
    pure $ liveDay o h l c v