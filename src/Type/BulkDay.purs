module Type.BulkDay where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String (length)
import Utils (allTrue, isAlphaStr)
import Railroad (rightToMaybe)
import Data.Argonaut.Parser (jsonParser)
import Data.Argonaut.Core (toNumber, toObject, toString)
import Foreign.Object (lookup)

type BulkDay = { code :: String, date :: String, open :: Number, high :: Number, low :: Number, close :: Number, volume :: Number }

bulkDay :: String -> String -> Number -> Number -> Number -> Number -> Number -> BulkDay
bulkDay = { code: _, date: _, open: _, high: _, low: _, close: _, volume: _ }

bulkDayFromJSON :: String -> Maybe BulkDay
bulkDayFromJSON jsonStr = 
  let 
    obj = (rightToMaybe $ jsonParser jsonStr) >>= toObject
    co = obj >>= lookup "code" >>= toString
    d = obj >>= lookup "date" >>= toString
    o = obj >>= lookup "open" >>= toNumber
    h = obj >>= lookup "high" >>= toNumber
    l = obj >>= lookup "low" >>= toNumber
    cl = obj >>= lookup "close" >>= toNumber
    v = obj >>= lookup "volume" >>= toNumber
  in bulkDay <$> co <*> d <*> o <*> h <*> l <*> cl <*> v

bulkDaysFromJSON :: String -> Maybe (Array BulkDay)
bulkDaysFromJSON _ = Nothing

isOptimalBulkDay :: BulkDay -> Boolean
isOptimalBulkDay day = 
  allTrue [length day.code <= 4, isAlphaStr day.code, (day.close * day.volume) >= 1000000.0]