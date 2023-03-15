module Type.Analysis 
  ( Analysis(..)
  , minAnalysisInputLength 
  )
  where

import Prelude

import Type.Indicator (Indicator, minIndInputLength)
import Type.Evaluator (Evaluator, minEvalInputLength)
import Data.Maybe (Maybe(..))

data Analysis a b c = Analysis (Indicator (Maybe a)) (Evaluator (a -> b)) (b -> c)

minAnalysisInputLength :: forall a b c. Analysis a b c -> Int
minAnalysisInputLength (Analysis i e _) = (minIndInputLength i) + (minEvalInputLength e)