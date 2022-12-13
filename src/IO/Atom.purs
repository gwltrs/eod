module IO.Atom where

import Prelude

import Control.Monad.Except (ExceptT(..))
import Effect (Effect)
import Effect.Aff (Aff, attempt, error)
import Fetch (fetch)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, writeTextFile)
import Node.Process (lookupEnv)
import Railroad (fromJust_, toRight, tryAff, tryEffect)
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

readFileText :: String -> AffE String
readFileText = readTextFile UTF8 >>> tryAff

writeFileText :: String -> String -> AffE Unit
writeFileText path text = writeTextFile UTF8 path text # tryAff