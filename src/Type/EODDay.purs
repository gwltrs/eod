module Type.EODDay where

import Prelude

import Data.Argonaut.Core (Json, toArray, toNumber, toObject, toString)
import Data.Argonaut.Parser (jsonParser)
import Data.Maybe (Maybe)
import Foreign.Object (lookup)
import Railroad (filterMapAll, rightToMaybe)
import Type.LiveDay (LiveDay)

type EODDay = { date :: String, open :: Number, high :: Number, low :: Number, close :: Number, volume :: Number }

eodDay :: String -> Number -> Number -> Number -> Number -> Number -> EODDay
eodDay = { date: _, open: _, high: _, low: _, close: _, volume: _ }

eodDayFromJSON :: Json -> Maybe EODDay
eodDayFromJSON json = 
  let 
    obj = toObject json
    d = obj >>= lookup "date" >>= toString
    o = obj >>= lookup "open" >>= toNumber
    h = obj >>= lookup "high" >>= toNumber
    l = obj >>= lookup "low" >>= toNumber
    c = obj >>= lookup "close" >>= toNumber
    v = obj >>= lookup "volume" >>= toNumber
  in eodDay <$> d <*> o <*> h <*> l <*> c <*> v

eodDaysFromJSON :: String -> Maybe (Array EODDay)
eodDaysFromJSON json = jsonParser json # rightToMaybe >>= toArray >>= filterMapAll eodDayFromJSON

toLiveDay :: EODDay -> LiveDay
toLiveDay d = { open: d.open, high: d.high, low: d.low, close: d.close, volume: d.volume }