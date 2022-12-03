module Type.BulkDay where

import Prelude

import Data.Argonaut.Core (Json, toArray, toNumber, toObject, toString)
import Data.Argonaut.Parser (jsonParser)
import Data.Maybe (Maybe(..))
import Data.String (length)
import Fetch.Core.Response (json)
import Foreign.Object (Object, lookup)
import Railroad (allOrNothing, filterMapAll, rightToMaybe)
import Utils (allTrue, isAlphaStr)

type BulkDay = { code :: String, date :: String, open :: Number, high :: Number, low :: Number, close :: Number, volume :: Number }

bulkDay :: String -> String -> Number -> Number -> Number -> Number -> Number -> BulkDay
bulkDay = { code: _, date: _, open: _, high: _, low: _, close: _, volume: _ }

bulkDayFromJSONObj :: Object Json -> Maybe BulkDay
bulkDayFromJSONObj obj = 
  let 
    --obj = (rightToMaybe $ jsonParser jsonStr) >>= toObject
    co = lookup "code" obj >>= toString
    d = lookup "date" obj >>= toString
    o = lookup "open" obj >>= toNumber
    h = lookup "high" obj >>= toNumber
    l = lookup "low" obj >>= toNumber
    cl = lookup "close" obj >>= toNumber
    v = lookup "volume" obj >>= toNumber
  in bulkDay <$> co <*> d <*> o <*> h <*> l <*> cl <*> v

bulkDaysFromJSON :: String -> Maybe (Array BulkDay)
bulkDaysFromJSON json = jsonParser json # rightToMaybe >>= toArray >>= filterMapAll toObject >>= filterMapAll bulkDayFromJSONObj

isOptimalBulkDay :: BulkDay -> Boolean
isOptimalBulkDay day = 
  allTrue [length day.code <= 4, isAlphaStr day.code, (day.close * day.volume) >= 1000000.0]