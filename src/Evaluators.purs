module Evaluators where

import Prelude
import Data.Foldable (class Foldable)
import Utils (undefined)

expectancy :: forall f. Foldable f => f Number -> Number
expectancy rMultiples = -1


--systemQuality :: Number -> Number -> Number
--systemQuality = undefined