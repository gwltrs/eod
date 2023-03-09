module Evaluators where

import Prelude

import Data.Array (filter, length)
import Data.Int (toNumber)
import Data.Foldable (class Foldable, foldr)
import Utils (undefined)

expectancy :: Array Number -> Number
expectancy rMultiples = 
  let 
    winners = filter (_ > 0.0) rMultiples
    losers = filter (_ < 0.0) rMultiples
    fLen arr = toNumber $ length arr
    avg arr = (foldr (+) 0.0 arr) / (fLen arr)
    calc [] [] = -2.0
    calc [] l = -2.0
    calc w [] = -2.0
    calc w l = 
      let 
        avgProfit = avg w
        avgLoss = avg l
        probWin = (fLen w) / (fLen rMultiples)
        probLoss = (fLen l) / (fLen rMultiples)
      in
        (avgProfit * probWin) - (avgLoss * probLoss)
  in
    calc winners losers

--systemQuality :: Number -> Number -> Number
--systemQuality = undefined