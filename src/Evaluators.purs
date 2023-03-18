module Evaluators where

import Prelude

import Data.Array (filter, length)
import Data.Int (toNumber)
import Data.Foldable (class Foldable, foldr)
import Data.Ord (abs)
import Data.Number (pow, sqrt)
import Utils (undefined)
import Type.Alias (RMultiple)
import Type.Evaluator (Evaluator(..), first)
import Type.Purchase (Purchase(..))
import Type.Day (Day(..))
import Forceable (frc)
import Data.Slice (slast)

type ExitStrategy = Evaluator (Purchase -> RMultiple)

at :: Int -> Evaluator Day
at i = 
  let i' = max 0 i
  in (frc <<< slast) <$> first (1 + i')

-- account for gap downs
maxPreviousLow :: ExitStrategy
maxPreviousLow = pure undefined