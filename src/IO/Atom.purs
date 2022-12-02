module IO.Atom where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Exception (throw)
import Fetch (fetch)
import Node.Process (lookupEnv)
import Type.Alias (URL)

getURL :: URL -> Aff String
getURL url = do
  res <- fetch url {}
  res.text

getAPIKey :: Effect String
getAPIKey = do
  keyMaybe <- lookupEnv envVarName
  case keyMaybe of
    Just key -> pure key
    Nothing -> throw ("Failed to get env var: " <> envVarName)
  where 
    envVarName = "EODHD_API_KEY"