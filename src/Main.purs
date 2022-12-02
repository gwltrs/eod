module Main where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import IO.Atom (getAPIKey, getURL)

printCatFact :: Aff Unit
printCatFact = do
  res <- getURL "https://catfact.ninja/fact"
  liftEffect (log res)

main ∷ Effect Unit
main = launchAff_ printCatFact
--main = getAPIKey >>= log