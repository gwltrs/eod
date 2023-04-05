module Type.JSON.EODDay (EODDay(..), eodDay, eodDaysFromJSON, eodDaysToJSON) where

import Prelude

import Data.Argonaut.Core (Json, toArray, toNumber, toObject, toString)
import Data.Argonaut.Parser (jsonParser)
import Data.Maybe (Maybe)
import Data.Traversable (traverse)
import Foreign.Object (lookup)
import Railroad (rightToMaybe)
import Type.YMD (YMD(..))
import Type.YMD as Y
import Utils (toJSONArray)
import Type.Alias (Ticker)

type EODDay = 
  { date :: YMD
  , open :: Number
  , high :: Number
  , low :: Number
  , close :: Number
  , volume :: Number
  }

eodDay :: YMD -> Number -> Number -> Number -> Number -> Number -> EODDay
eodDay = { date: _, open: _, high: _, low: _, close: _, volume: _ }

eodDayFromJSON :: Json -> Maybe EODDay
eodDayFromJSON json = do
  obj <- toObject json
  d <- lookup "date" obj >>= toString >>= Y.parse
  o <- lookup "open" obj >>= toNumber
  h <- lookup "high" obj >>= toNumber
  l <- lookup "low" obj >>= toNumber
  c <- lookup "close" obj >>= toNumber
  v <- lookup "volume" obj >>= toNumber
  pure $ eodDay d o h l c v

eodDaysFromJSON :: String -> Maybe (Array EODDay)
eodDaysFromJSON json = jsonParser json # rightToMaybe >>= toArray >>= traverse eodDayFromJSON

eodDayToJSON :: EODDay -> String
eodDayToJSON day = 
  "{\"code\":\"" <> "\"" <>
  ",\"date\":\"" <> show day.date <> "\"" <>
  ",\"open\":" <> show day.open <>
  ",\"high\":" <> show day.high <>
  ",\"low\":" <> show day.low <>
  ",\"close\":" <> show day.close <>
  ",\"volume\":" <> show day.volume <> "}"

eodDaysToJSON :: Array EODDay -> String
eodDaysToJSON = toJSONArray eodDayToJSON