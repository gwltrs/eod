module Type.Analysis 
  ( Analysis(..)
  , minAnalysisInputLength 
  )
  where

import Prelude

import Type.Indicator (Indicator, minIndInputLength)
import Type.Evaluator (Evaluator, minEvalInputLength)

data Analysis a b c = Analysis (Indicator a) (Evaluator (a -> b)) c (b -> c -> c)

minAnalysisInputLength :: forall a b c. Analysis a b c -> Int
minAnalysisInputLength (Analysis i e _ _) = (minIndInputLength i) + (minEvalInputLength e)