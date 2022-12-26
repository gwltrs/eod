module Main where

import Prelude

import Control.Apply ((*>))
import Control.Monad.Except (ExceptT(..), runExceptT)
import Data.Array (length)
import Data.Bifunctor (bimap)
import Data.Date (Date)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, Error, launchAff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import IO (getBulkDays, getEODDays, getLiveDay)
import Railroad (fuse, launchAffE)
import Type.Alias (AffE)
import Type.Date as D

previousTradingDate :: Date
previousTradingDate = D.unsafeDate 2022 12 5

logAffE :: forall a. (Error -> Maybe String) -> (a -> Maybe String) -> AffE a -> AffE a
logAffE logErr logA aff = runExceptT aff >>= (\eith ->
  case eith of
    Left l -> case logErr l of
      Just e -> (liftEffect $ log e) *> (pure $ Left l)
      Nothing -> pure $ Left l
    Right r -> case logA r of
      Just a -> (liftEffect $ log a) *> (pure $ Right r)
      Nothing -> pure $ Right r) # ExceptT

main ∷ Effect Unit
main = getBulkDays (D.unsafeDate 2022 12 5) # logAffE (show >>> Just) (length >>> show >>> Just) # launchAffE