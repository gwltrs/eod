module Type.DayWithCode where

import Prelude

type DayWithCode = { code :: String, date :: String, open :: Number, high :: Number, low :: Number, close :: Number, volume :: Number }

dayWithCode :: String -> String -> Number -> Number -> Number -> Number -> Number -> DayWithCode
dayWithCode = { code: _, date: _, open: _, high: _, low: _, close: _, volume: _ }