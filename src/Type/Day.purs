module Type.Day where

type Day = { date :: String, open :: Number, high :: Number, low :: Number, close :: Number, volume :: Number }

day :: String -> Number -> Number -> Number -> Number -> Number -> Day
day d o h l c v = { date: d, open: o, high: h, low: l, close: c, volume: v }