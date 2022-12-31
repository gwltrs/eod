module Forceable where

import Prelude

import Data.Array (index, unsafeIndex)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromJust)
import Data.Slice (Slice, sat, slen)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)

class Forceable f where
  frc :: forall a. f a -> a

instance forceableMaybe :: Forceable Maybe where
  frc (Just a) = a
  frc Nothing = unsafeCrashWith "failed to force Maybe"

instance forceableEither :: Forceable (Either a) where
  frc (Right r) = r
  frc (Left _) = unsafeCrashWith "failed to force Either"

instance forceableArray :: Forceable Array where
  frc [] = unsafeCrashWith "failed to force Array"
  frc a = unsafePartial fromJust $ index a 0

instance forceableSlice :: Forceable Slice where
  frc s = 
    if slen s > 0 
    then unsafePartial fromJust $ sat s 0 
    else unsafeCrashWith "failed to force Slice"

forceApply :: forall a b c. Forceable a => (b -> c) -> a b -> c
forceApply f x = f (frc x)

infixr 0 forceApply as $!

forceApplyFlipped :: forall a b c. a b -> Forceable a => (b -> c) -> c
forceApplyFlipped x f = f (frc x)

infixl 1 forceApplyFlipped as !#