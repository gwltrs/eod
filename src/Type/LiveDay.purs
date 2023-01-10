module Type.LiveDay
  ( LiveDay
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

type LiveDay = { open :: Number, high :: Number, low :: Number, close :: Number, volume :: Number }

liveDay :: Number -> Number -> Number -> Number -> Number -> LiveDay
liveDay = { open: _, high: _, low: _, close: _, volume: _ }

liveDayFromJSON :: String -> Maybe LiveDay
liveDayFromJSON json = do
  obj <- jsonParser json # rightToMaybe >>= toObject
  o <- lookup "open" obj >>= toNumber
  h <- lookup "high" obj >>= toNumber
  l <- lookup "low" obj >>= toNumber
  c <- lookup "close" obj >>= toNumber
  v <- lookup "volume" obj >>= toNumber
  pure $ liveDay o h l c v

noMove :: Number -> LiveDay
noMove n = { open: n, high: n, low: n, close: n, volume: n }

avg :: LiveDay -> Number
avg d = (d.open + d.high + d.low + d.close) / 4.0