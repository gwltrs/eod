module Type.AffE where

import Prelude
import Effect.Aff (Aff)
import Type.AppError (AppError)
import Control.Monad.Except (ExceptT)
import Effect.Class (class MonadEffect)
import Effect.Class as EC
import Utils (undefined)
import Type.EffectE (EffectE)
import Effect (Effect)
import Effect.Exception (Error)
import Data.Maybe (Maybe(..))
import Control.Monad.Except (ExceptT(..), runExceptT)
import Effect.Aff (Aff, attempt, launchAff_)
import Data.Bifunctor (lmap, bimap)
import Control.Monad.Except.Trans (lift, mapExceptT)
import Data.Either (Either(..), either)

type AffE a = ExceptT AppError Aff a

launch :: forall a. (AppError -> Aff Unit) -> AffE Unit -> Effect Unit
launch handleError affe = runExceptT affe
  >>= (either handleError pure)
  # launchAff_

liftAff :: forall a. Aff a -> AffE a
liftAff = lift

liftEffect :: forall a. Effect a -> AffE a
liftEffect = EC.liftEffect 

liftEffectE :: forall a. EffectE a -> AffE a
liftEffectE = mapExceptT EC.liftEffect

liftMaybe :: forall a. AppError -> Maybe a -> AffE a
liftMaybe _ (Just a) = pure a
liftMaybe appErr Nothing = ExceptT $ pure $ Left appErr

tryAff :: forall a. (Error -> AppError) -> Aff a -> AffE a
tryAff toAppError aff = ExceptT $ (lmap toAppError) <$> attempt aff