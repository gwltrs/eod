module Evaluators where

import Prelude

import Data.Array (filter, length)
import Data.Int (toNumber)
import Data.Foldable (class Foldable, foldr)
import Data.Ord (abs)
import Data.Number (pow, sqrt)
import Utils (undefined)
import Type.Alias (RMultiple)
import Type.Evaluator (Evaluator, first)
import Type.Purchase (Purchase(..), buyPrice, stopPrice)
import Type.Day (Day(..), open, close, low, day)
import Forceable (frc)
import Data.Slice (slast)
import Data.Slice (Slice(..), slen, sat)

type ExitStrategy = Evaluator (Purchase -> RMultiple)

at :: Int -> Evaluator Day
at i = 
  let i' = max 0 i
  in (frc <<< slast) <$> first (1 + i')

maxPreviousLow :: Int -> ExitStrategy
maxPreviousLow n = 
  let
    findExit :: Number -> Number -> Int -> Slice Day -> Number
    findExit purchase stop index days = 
      if index >= (slen days)
      then purchase
      else 
        if (open (frc $ sat days index)) <= stop
        then open (frc $ sat days index)
        else 
          if low (frc $ sat days index) <= stop
          then stop
          else findExit 
            (close (frc $ sat days index)) 
            (max stop (low (frc $ sat days index))) 
            (index + 1) 
            days
      
    f :: Slice Day -> (Purchase -> RMultiple)
    f days purchase =
      let 
        b = buyPrice purchase
        s = stopPrice purchase
        r :: RMultiple
        r = b - s
      in
        (findExit b s 0 days - b) / r

  in
    f <$> first n