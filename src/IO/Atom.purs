module IO.Atom where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Exception (throw)
import Fetch (fetch)
import Node.Process (lookupEnv)
import Railroad (forceJust)
import Type.Alias (URL)

getURL :: URL -> Aff String
getURL url = fetch url {} >>= (\r -> r.text)

getAPIKey :: Effect String
getAPIKey = 
  let 
    envVarName = "EODHD_API_KEY"
    err = "Didn't find environment variable: " <> envVarName
  in 
    lookupEnv envVarName <#> forceJust err