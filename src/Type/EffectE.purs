module Type.EffectE where

import Prelude
import Effect (Effect)
import Type.AppError (AppError)
import Control.Monad.Except (ExceptT)
import Effect.Class (class MonadEffect)
import Utils (undefined)
import Data.Maybe (Maybe)

type EffectE a = ExceptT AppError Effect a

liftEffect :: forall a. Effect a -> EffectE a
liftEffect = undefined

liftEffectMaybe :: forall a. AppError -> Effect (Maybe a) -> EffectE a
liftEffectMaybe = undefined