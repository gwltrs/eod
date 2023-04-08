module EffectEs where

import Prelude
import Type.EffectE (EffectE)
import Type.EffectE as EE
import Node.Process (lookupEnv)
import Type.AppError (AppError(..))

getAPIKey :: EffectE String
getAPIKey = EE.liftEffectMaybe MissingAPIKey (lookupEnv "EODHD_API_KEY")