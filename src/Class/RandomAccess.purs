module Class.RandomAccess where

import Prelude

import Data.Array (length, (!!))
import Data.Slice (Slice, sat, slen)
import Forceable (frc)

class RandomAccess r where
  rLen :: forall a. r a -> Int
  rAt :: forall a. Int -> r a -> a

instance randomAccessArray :: RandomAccess Array where
  rLen = length
  rAt i a = frc (a !! i)

instance randomAccessSlice :: RandomAccess Slice where
  rLen = slen
  rAt i s = frc $ sat s i