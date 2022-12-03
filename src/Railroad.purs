module Railroad where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isJust)
import Data.Array (all)
import Partial.Unsafe (unsafeCrashWith)

rightToMaybe :: forall a b. Either a b -> Maybe b
rightToMaybe (Left _) = Nothing
rightToMaybe (Right b) = Just b

leftToMaybe :: forall a b. Either a b -> Maybe a
leftToMaybe (Left a) = Just a
leftToMaybe (Right _) = Nothing

fromJust_ :: forall a. String -> Maybe a -> a
fromJust_ _ (Just a) = a
fromJust_ err (Nothing) = unsafeCrashWith err

fromJust :: forall a. Maybe a -> a
fromJust = fromJust_ "Expecting a Just but found Nothing"

filterMapAll :: forall a b. (a -> Maybe b) -> Array a -> Maybe (Array b)
filterMapAll f arr = 
  let mapped = arr <#> f
  in if all isJust mapped then Just (mapped <#> fromJust) else Nothing

allOrNothing :: forall a. Array (Maybe a) -> Maybe (Array a)
allOrNothing = filterMapAll identity