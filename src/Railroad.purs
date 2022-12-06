module Railroad where

import Prelude

import Control.Apply (applySecond)
import Control.Monad.Error.Class (try)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Data.Array (all)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isJust)
import Effect (Effect)
import Effect.Aff (Aff, attempt, launchAff_)
import Effect.Class (liftEffect)
import Partial.Unsafe (unsafeCrashWith)
import Type.Alias (EffectE, AffE)

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

tryEffect :: forall a. Effect a -> EffectE a
tryEffect = try >>> ExceptT

tryAff :: forall a. Aff a -> AffE a
tryAff = attempt >>> ExceptT

launchAffE :: forall a. AffE a -> Effect Unit
launchAffE a = launchAff_ (runExceptT a `applySecond` pure unit)

toRight :: forall e a. e -> Maybe a -> Either e a
toRight e Nothing = Left e 
toRight _ (Just a) = Right a

liftEffectE :: forall a. EffectE a -> AffE a
liftEffectE = runExceptT >>> liftEffect >>> ExceptT

fuse :: forall a. Either a a -> a
fuse (Left l) = l
fuse (Right r) = r