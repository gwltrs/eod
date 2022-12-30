module Type.Indicator where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Slice (Slice)
import Type.LiveDay (LiveDay)

indicate :: forall a. Indicator a -> Slice LiveDay -> Maybe a
indicate (Indicator i) s = Just $ i.f s 

newtype Indicator a = Indicator { n :: Int, f :: Slice LiveDay -> a }

instance functorIndicator :: Functor Indicator where
  map f (Indicator i) = Indicator { n: i.n, f: i.f >>> f }

instance applyIndicator :: Apply Indicator where
  apply (Indicator f) (Indicator i) = Indicator { n: max f.n i.n, f: (\d -> f.f d $ i.f d) }

instance applicativeIndicator :: Applicative Indicator where
  pure a = Indicator { n: 0, f: const a }