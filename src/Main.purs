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
import Forceable (frc)
import IO (getBulkDays, getEODDays, getLiveDay, logAffE)
import Railroad (fuse, launchAffE)
import Type.Alias (AffE)
import Type.YMD (YMD(..), ymd)

previousTradingDate :: YMD
previousTradingDate = frc $ ymd 2022 12 23

main ∷ Effect Unit
main = getBulkDays previousTradingDate <#> length # logAffE # launchAffE