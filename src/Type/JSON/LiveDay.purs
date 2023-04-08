module Type.JSON.LiveDay where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Argonaut.Parser (jsonParser)
import Data.Argonaut.Core (Json, toArray, toNumber, toObject, toString)
import Foreign.Object (lookup)
import Utils (right)

type LiveDay = 
  { open :: Number
  , high :: Number
  , low :: Number
  , close :: Number
  , volume :: Number
  }
  
liveDayFromJSON :: String -> Maybe LiveDay
liveDayFromJSON json = do
  obj <- jsonParser json # right >>= toObject
  o <- lookup "open" obj >>= toNumber
  h <- lookup "high" obj >>= toNumber
  l <- lookup "low" obj >>= toNumber
  c <- lookup "close" obj >>= toNumber
  v <- lookup "volume" obj >>= toNumber
  pure $ { open: o, high: h, low: l, close: c, volume: v }

--instance showLiveDay :: Show LiveDay where
--  show d = "LiveDay " <> show d