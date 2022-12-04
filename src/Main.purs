module Main where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import IO (getLiveDay)
import IO.Atom (getAPIKey, getURL)

printCatFact :: Aff Unit
printCatFact = do
  --res <- getURL "https://catfact.ninja/fact"
  res <- getLiveDay "AMZN"
  liftEffect (log $ show $ res)

main ∷ Effect Unit
main = launchAff_ printCatFact