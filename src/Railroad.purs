module RailRoad where

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Partial.Unsafe (unsafeCrashWith)

rightToMaybe :: forall a b. Either a b -> Maybe b
rightToMaybe (Left _) = Nothing
rightToMaybe (Right b) = Just b

leftToMaybe :: forall a b. Either a b -> Maybe a
leftToMaybe (Left a) = Just a
leftToMaybe (Right _) = Nothing

forceJust :: forall a. String -> Maybe a -> a
forceJust _ (Just a) = a
forceJust err (Nothing) = unsafeCrashWith err