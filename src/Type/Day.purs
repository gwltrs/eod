module Type.Day where

type Day = { date :: String, open :: Number, high :: Number, low :: Number, close :: Number, volume :: Number }

day :: String -> Number -> Number -> Number -> Number -> Number -> Day
day = { date: _, open: _, high: _, low: _, close: _, volume: _ }