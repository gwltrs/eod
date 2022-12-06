module Main where

import Prelude

import Control.Apply (applySecond)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Data.Array (length)
import Data.Bifunctor (bimap)
import Data.Date (Date)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, Error, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import IO (getBulkDays, getEODDays, getLiveDay)
import Railroad (fromJust, fuse, launchAffE)
import Type.Alias (AffE)
import Utils (date)

printCatFact :: AffE Unit
printCatFact = do
  --res <- getURL "https://catfact.ninja/fact"
  res <- getLiveDay "AMZN"
  liftEffect $ log $ show $ res

--logAffE :: String -> AffE Unit
--logAffE = log >>> laun

previousTradingDate :: Date
previousTradingDate = fromJust $ date 2022 12 5

logAffE :: forall a. (Error -> Maybe String) -> (a -> Maybe String) -> AffE a -> AffE a
logAffE logErr logA aff = runExceptT aff >>= (\eith ->
  case eith of
    Left l -> case logErr l of
      Just e -> (liftEffect $ log e) `applySecond` (pure $ Left l)
      Nothing -> pure $ Left l
    Right r -> case logA r of
      Just a -> (liftEffect $ log a) `applySecond` (pure $ Right r)
      Nothing -> pure $ Right r) # ExceptT

main ∷ Effect Unit
main = getBulkDays (fromJust $ date 2022 12 5) # logAffE (show >>> Just) (length >>> show >>> Just) # launchAffE