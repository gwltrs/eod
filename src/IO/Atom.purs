module IO.Atom where

import Prelude

import Control.Monad.Except (ExceptT(..))
import Effect (Effect)
import Effect.Aff (Aff, attempt, error)
import Fetch (fetch)
import Node.Process (lookupEnv)
import Railroad (fromJust_, tryEffect, toRight)
import Type.Alias (AffE, URL, EffectE)

getURL :: URL -> AffE String
getURL url = ExceptT $ attempt (fetch url {} >>= (\r -> r.text))

getAPIKey :: EffectE String
getAPIKey = 
  let 
    envVarName = "EODHD_API_KEY"
    err = error ("Didn't find environment variable: " <> envVarName)
  in 
    lookupEnv envVarName <#> toRight err # ExceptT