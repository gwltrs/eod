module Type.Indicator
  ( Indicator
  , indicate
  , indicate'
  , last
  , lastShifted
  , minIndInputLength
  )
  where

import Prelude

import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import Data.Slice (Slice, slen, slice, stake)
import Type.Day (Day)
import Utils (sdrop, slastN, undefined)

data Indicator a = Indicator Int (Slice Day -> a)

last :: Int -> Indicator (Slice Day)
last n = Indicator n (slastN n)

lastShifted :: Int -> Int -> Indicator (Slice Day)
lastShifted n shiftBy = 
  let total = n + shiftBy
  in Indicator total (stake n <<< slastN total)

indicate :: forall a. Indicator a -> Slice Day -> Maybe a
indicate (Indicator n f) s = if slen s >= n then Just $ f s else Nothing

indicate' :: forall a. Indicator a -> Array Day -> Maybe a
indicate' i a = indicate i (slice a)

minIndInputLength :: forall a. Indicator a -> Int
minIndInputLength (Indicator n _) = n

instance functorIndicator :: Functor Indicator where
  map f' (Indicator n f) = Indicator n (map f' f)

instance applyIndicator :: Apply Indicator where
  apply (Indicator lN lF) (Indicator rN rF) = Indicator (max lN rN) (apply lF rF)

instance applicativeIndicator :: Applicative Indicator where
  pure a = Indicator 0 (pure a)