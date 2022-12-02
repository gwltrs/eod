module RailRoad where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Either (Either(..))

rightToMaybe :: forall a b. Either a b -> Maybe b
rightToMaybe (Left _) = Nothing
rightToMaybe (Right b) = Just b

leftToMaybe :: forall a b. Either a b -> Maybe a
leftToMaybe (Left a) = Just a
leftToMaybe (Right _) = Nothing