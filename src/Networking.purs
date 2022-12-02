module Networking (parseDay) where

import Prelude

import Data.Argonaut.Core (toNumber, toObject, toString)
import Data.Argonaut.Parser (jsonParser)
import Data.Maybe (Maybe)
import Foreign.Object (lookup)
import RailRoad (rightToMaybe)
import Type.Day (Day, day)

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

