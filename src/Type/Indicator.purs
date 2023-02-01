module Type.Indicator
  ( Indicator
  , indicate
  , indicate'
  , minimumInputLength
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

--indicator :: forall a. Int -> Int -> (Slice Day -> a) -> Indicator a
--indicator l o f = Indicator { len: l, offset: o, f: slastN n >>> f }

indicate :: forall a. Indicator a -> Slice Day -> Maybe a
indicate _ = undefined
--indicate (Indicator i) s = if slen s >= i.n then Just $ i.f s else Nothing

indicate' :: forall a. Indicator a -> Array Day -> Maybe a
indicate' i a = indicate i (slice a)

minimumInputLength :: forall a. Indicator a -> Int
minimumInputLength (Indicator n _) = n

--shiftLeft :: forall a. Int -> Indicator a -> Indicator a
--shiftLeft n (Indicator i) = Indicator { n: (i.n + n), f: sdrop n >>> i.f }

--shiftLeftFlipped :: forall a. Indicator a -> Int -> Indicator a
--shiftLeftFlipped = flip shiftLeft

--infixr 9 shiftLeftFlipped as <<

instance functorIndicator :: Functor Indicator where
  map f' (Indicator n f) = Indicator n (map f' f)

instance applyIndicator :: Apply Indicator where
  apply (Indicator a b) (Indicator c d) = Indicator (max a c) (apply b d) -- (Indicator f) (Indicator i) = Indicator { n: max f.n i.n, f: (\d -> f.f d $ i.f d) }

instance applicativeIndicator :: Applicative Indicator where
  pure a = Indicator 0 (pure a) -- undefinedIndicator

-- instance bindIndicator :: Bind Indicator where
--   bind (Indicator a b) c = Indicator 

-- instance monadIndicator :: Monad Indicator