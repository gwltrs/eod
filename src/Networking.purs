module Networking where

import Prelude

import Data.Argonaut.Core (toNumber, toObject, toString)
import Data.Argonaut.Parser (jsonParser)
import Data.Maybe (Maybe)
import Foreign.Object (lookup)
import RailRoad (rightToMaybe)
import Type.Day (Day, day)
import Type.DayWithCode (DayWithCode, dayWithCode)
import Utils (allTrue, isAlphaStr)
import Data.String (length)

parseDay :: String -> Maybe Day
parseDay jsonStr = 
  let 
    obj = (rightToMaybe $ jsonParser jsonStr) >>= toObject
    d = obj >>= lookup "date" >>= toString
    o = obj >>= lookup "open" >>= toNumber
    h = obj >>= lookup "high" >>= toNumber
    l = obj >>= lookup "low" >>= toNumber
    c = obj >>= lookup "close" >>= toNumber
    v = obj >>= lookup "volume" >>= toNumber
  in day <$> d <*> o <*> h <*> l <*> c <*> v

parseDayWithCode :: String -> Maybe DayWithCode
parseDayWithCode jsonStr = 
  let 
    obj = (rightToMaybe $ jsonParser jsonStr) >>= toObject
    co = obj >>= lookup "code" >>= toString
    d = obj >>= lookup "date" >>= toString
    o = obj >>= lookup "open" >>= toNumber
    h = obj >>= lookup "high" >>= toNumber
    l = obj >>= lookup "low" >>= toNumber
    cl = obj >>= lookup "close" >>= toNumber
    v = obj >>= lookup "volume" >>= toNumber
  in dayWithCode <$> co <*> d <*> o <*> h <*> l <*> cl <*> v

isOptimalBulkDay :: DayWithCode -> Boolean
isOptimalBulkDay day = 
  allTrue [length day.code <= 4, isAlphaStr day.code, (day.close * day.volume) >= 1000000.0]