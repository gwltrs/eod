module Type.EODDay (EODDay, eodDay, eodDaysFromJSON, toLiveDay) where

import Prelude

import Data.Argonaut.Core (Json, toArray, toNumber, toObject, toString)
import Data.Argonaut.Parser (jsonParser)
import Data.Maybe (Maybe)
import Data.Traversable (traverse)
import Foreign.Object (lookup)
import Railroad (rightToMaybe)
import Type.LiveDay (LiveDay)

type EODDay = { date :: String, open :: Number, high :: Number, low :: Number, close :: Number, volume :: Number }

eodDay :: String -> Number -> Number -> Number -> Number -> Number -> EODDay
eodDay = { date: _, open: _, high: _, low: _, close: _, volume: _ }

eodDayFromJSON :: Json -> Maybe EODDay
eodDayFromJSON json = do
  obj <- toObject json
  d <- lookup "date" obj >>= toString
  o <- lookup "open" obj >>= toNumber
  h <- lookup "high" obj >>= toNumber
  l <- lookup "low" obj >>= toNumber
  c <- lookup "close" obj >>= toNumber
  v <- lookup "volume" obj >>= toNumber
  pure $ eodDay d o h l c v

eodDaysFromJSON :: String -> Maybe (Array EODDay)
eodDaysFromJSON json = jsonParser json # rightToMaybe >>= toArray >>= traverse eodDayFromJSON

toLiveDay :: EODDay -> LiveDay
toLiveDay d = { open: d.open, high: d.high, low: d.low, close: d.close, volume: d.volume }