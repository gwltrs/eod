module Type.LiveDay (LiveDay, liveDay, liveDayFromJSON) where

import Prelude

import Data.Argonaut.Core (Json, toArray, toNumber, toObject, toString)
import Data.Argonaut.Parser (jsonParser)
import Data.Maybe (Maybe(..))
import Foreign.Object (lookup)
import Railroad (filterMapAll, rightToMaybe)

type LiveDay = { open :: Number, high :: Number, low :: Number, close :: Number, volume :: Number }

liveDay :: Number -> Number -> Number -> Number -> Number -> LiveDay
liveDay = { open: _, high: _, low: _, close: _, volume: _ }

liveDayFromJSON :: String -> Maybe LiveDay
liveDayFromJSON json =
  let 
    obj = jsonParser json # rightToMaybe >>= toObject
    o = obj >>= lookup "open" >>= toNumber
    h = obj >>= lookup "high" >>= toNumber
    l = obj >>= lookup "low" >>= toNumber
    c = obj >>= lookup "close" >>= toNumber
    v = obj >>= lookup "volume" >>= toNumber
  in 
    liveDay <$> o <*> h <*> l <*> c <*> v