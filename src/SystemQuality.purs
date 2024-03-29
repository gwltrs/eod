module SystemQuality 
  ( expectancy
  , standardDeviation
  , systemQuality
  , SQN
  ) where

import Prelude
import Type.Alias (RMultiple)
import Data.Array (filter, length)
import Data.Int (toNumber)
import Data.Foldable (class Foldable, foldr)
import Data.Ord (abs)
import Data.Number (pow, sqrt)

type SQN = Number -- System Quality Number

expectancy :: Array RMultiple -> Number
expectancy [] = 0.0
expectancy rMultiples = (foldr (+) 0.0 rMultiples) / (toNumber $ length rMultiples)

standardDeviation :: Array Number -> Number
standardDeviation [] = 0.0
standardDeviation ns = 
  let
    mean = foldr (+) 0.0 ns  / (toNumber $ length ns) 
    diffs = ns <#> (\x -> pow (x - mean) 2.0) # foldr (+) 0.0
    n = (toNumber $ length ns)
  in 
    sqrt (diffs / n)

systemQuality :: Array RMultiple -> SQN
systemQuality [] = 0.0
systemQuality rMultiples = 
  let 
    e = expectancy rMultiples
    std = max 1.0 (standardDeviation rMultiples)
    sqrtN = sqrt $ toNumber $ length rMultiples
  in
    (e / std) * sqrtN
