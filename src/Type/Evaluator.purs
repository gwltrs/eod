module Type.Evaluator 
  ( Evaluator
  , evaluate
  , evaluate'
  , first
  , minEvalInputLength
  )
  where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Slice (Slice, slen, slice, stake)
import Type.Day (Day)

data Evaluator a = Evaluator Int (Slice Day -> a)

first :: Int -> Evaluator (Slice Day)
first n = Evaluator n (stake n)

evaluate :: forall a. Evaluator a -> Slice Day -> Maybe a
evaluate (Evaluator n f) s = if slen s >= n then Just $ f s else Nothing

evaluate' :: forall a. Evaluator a -> Array Day -> Maybe a
evaluate' i a = evaluate i (slice a)

minEvalInputLength :: forall a. Evaluator a -> Int
minEvalInputLength (Evaluator n _) = n

instance functorEvaluator :: Functor Evaluator where
  map f' (Evaluator n f) = Evaluator n (map f' f)

instance applyEvaluator :: Apply Evaluator where
  apply (Evaluator lN lF) (Evaluator rN rF) = Evaluator (max lN rN) (apply lF rF)

instance applicativeEvaluator :: Applicative Evaluator where
  pure a = Evaluator 0 (pure a)