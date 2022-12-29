module Type.Indicator where

import Prelude

import Data.Slice (Slice)
import Type.LiveDay (LiveDay)

newtype Indicator a = Indicator { minNumDays :: Int, indicate :: (Slice LiveDay) -> a }

instance functorIndicator :: Functor Indicator where
  map f (Indicator i) = Indicator { minNumDays: i.minNumDays, indicate: i.indicate >>> f }

instance applyIndicator :: Apply Indicator where
  apply (Indicator f) (Indicator i) = Indicator { 
    minNumDays: max f.minNumDays i.minNumDays, 
    indicate: (\days -> f.indicate days $ i.indicate days) }

instance applicativeIndicator :: Applicative Indicator where
  pure a = Indicator { minNumDays: 0, indicate: const a }