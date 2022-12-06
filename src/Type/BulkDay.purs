module Type.BulkDay (BulkDay, bulkDay, bulkDaysFromJSON, isOptimalBulkDay, toEODDay) where

import Prelude

import Data.Argonaut.Core (Json, toArray, toNumber, toObject, toString)
import Data.Argonaut.Parser (jsonParser)
import Data.Maybe (Maybe)
import Data.String (length)
import Data.Traversable (traverse)
import Foreign.Object (lookup)
import Railroad (rightToMaybe)
import Type.EODDay (EODDay)
import Utils (allTrue, isAlphaStr)

type BulkDay = { code :: String, date :: String, open :: Number, high :: Number, low :: Number, close :: Number, volume :: Number }

bulkDay :: String -> String -> Number -> Number -> Number -> Number -> Number -> BulkDay
bulkDay = { code: _, date: _, open: _, high: _, low: _, close: _, volume: _ }

bulkDayFromJSON :: Json -> Maybe BulkDay
bulkDayFromJSON json = do
  obj <- toObject json
  co <- lookup "code" obj >>= toString
  d <- lookup "date" obj>>= toString
  o <- lookup "open" obj>>= toNumber
  h <- lookup "high" obj>>= toNumber
  l <- lookup "low" obj>>= toNumber
  cl <- lookup "close" obj>>= toNumber
  v <- lookup "volume" obj>>= toNumber
  pure $ bulkDay co d o h l cl v

bulkDaysFromJSON :: String -> Maybe (Array BulkDay)
bulkDaysFromJSON json = jsonParser json # rightToMaybe >>= toArray >>= traverse bulkDayFromJSON

isOptimalBulkDay :: BulkDay -> Boolean
isOptimalBulkDay day = 
  allTrue [length day.code <= 4, isAlphaStr day.code, (day.close * day.volume) >= 1000000.0]

toEODDay :: BulkDay -> EODDay
toEODDay d = { date: d.date, open: d.open, high: d.high, low: d.low, close: d.close, volume: d.volume }