module Type.AffE where

import Prelude
import Effect.Aff (Aff)
import Type.AppError (AppError)
import Control.Monad.Except (ExceptT)
import Effect.Class (class MonadEffect)
import Utils (undefined)
import Type.EffectE (EffectE)
import Effect (Effect)
import Effect.Exception (Error)
import Data.Maybe (Maybe)

type AffE a = ExceptT AppError Aff a

launch :: forall a. AffE a -> Effect Unit
launch = undefined

liftAff :: forall a. Aff a -> AffE a
liftAff = undefined

liftEffect :: forall a. Effect a -> AffE a
liftEffect = undefined

liftEffectE :: forall a. EffectE a -> AffE a
liftEffectE = undefined

liftMaybe :: forall a. AppError -> Maybe a -> AffE a
liftMaybe = undefined

tryAff :: forall a. (Error -> AppError) -> Aff a -> AffE a
tryAff = undefined