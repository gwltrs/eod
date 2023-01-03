module Type.Indicator
  ( (<<)
  , Indicator
  , indicate
  , indicate'
  , indicator
  , shiftLeft
  , shiftLeftFlipped
  )
  where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Slice (Slice, slen, slice, stake)
import Type.LiveDay (LiveDay)
import Utils (sdrop, slastN)

indicator :: forall a. Int -> (Slice LiveDay -> a) -> Indicator a
indicator n f = Indicator { n: n, f: slastN n >>> f }

indicate :: forall a. Indicator a -> Slice LiveDay -> Maybe a
indicate (Indicator i) s = if slen s >= i.n then Just $ i.f s else Nothing

indicate' :: forall a. Indicator a -> Array LiveDay -> Maybe a
indicate' i a = indicate i (slice a)

shiftLeft :: forall a. Int -> Indicator a -> Indicator a
shiftLeft n (Indicator i) = Indicator { n: (i.n + n), f: sdrop n >>> i.f }

shiftLeftFlipped :: forall a. Indicator a -> Int -> Indicator a
shiftLeftFlipped = flip shiftLeft

infixr 9 shiftLeftFlipped as <<

newtype Indicator a = Indicator { n :: Int, f :: Slice LiveDay -> a }

instance functorIndicator :: Functor Indicator where
  map f (Indicator i) = Indicator { n: i.n, f: i.f >>> f }

instance applyIndicator :: Apply Indicator where
  apply (Indicator f) (Indicator i) = Indicator { n: max f.n i.n, f: (\d -> f.f d $ i.f d) }

instance applicativeIndicator :: Applicative Indicator where
  pure a = Indicator { n: 0, f: const a }