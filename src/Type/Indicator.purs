module Type.Indicator
  ( Indicator
  , indicate
  , indicate'
  , indicator
  )
  where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Slice (Slice, slice)
import Type.LiveDay (LiveDay)

indicator :: forall a. Int -> (Slice LiveDay -> a) -> Indicator a
indicator n f = Indicator { n: n, f:  f }

indicate :: forall a. Indicator a -> Slice LiveDay -> Maybe a
indicate (Indicator i) s = Just $ i.f s 

indicate' :: forall a. Indicator a -> Array LiveDay -> Maybe a
indicate' i a = indicate i (slice a) 

newtype Indicator a = Indicator { n :: Int, f :: Slice LiveDay -> a }

instance functorIndicator :: Functor Indicator where
  map f (Indicator i) = Indicator { n: i.n, f: i.f >>> f }

instance applyIndicator :: Apply Indicator where
  apply (Indicator f) (Indicator i) = Indicator { n: max f.n i.n, f: (\d -> f.f d $ i.f d) }

instance applicativeIndicator :: Applicative Indicator where
  pure a = Indicator { n: 0, f: const a }