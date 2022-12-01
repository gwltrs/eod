module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Fetch (fetch)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)

printCatFact = do
  res <- fetch "https://catfact.ninja/fact" {}
  resText <- res.text
  liftEffect (log resText)

main = do
  launchAff_ printCatFact