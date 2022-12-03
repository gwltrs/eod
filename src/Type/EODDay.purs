module Type.EODDay where

import Prelude

import Data.Maybe (Maybe(..))
import Railroad (rightToMaybe)

type EODDay = { date :: String, open :: Number, high :: Number, low :: Number, close :: Number, volume :: Number }

eodDay :: String -> Number -> Number -> Number -> Number -> Number -> EODDay
eodDay = { date: _, open: _, high: _, low: _, close: _, volume: _ }

-- eodDayFromJSON :: String -> Maybe EODDay
-- eodDayFromJSON jsonStr = 
--   let 
--     obj = (rightToMaybe $ jsonParser jsonStr) >>= toObject
--     d = obj >>= lookup "date" >>= toString
--     o = obj >>= lookup "open" >>= toNumber
--     h = obj >>= lookup "high" >>= toNumber
--     l = obj >>= lookup "low" >>= toNumber
--     c = obj >>= lookup "close" >>= toNumber
--     v = obj >>= lookup "volume" >>= toNumber
--   in day <$> d <*> o <*> h <*> l <*> c <*> v

-- eodDaysFromJSON :: String -> Maybe (Array EODDay)
-- eodDaysFromJSON = Nothing