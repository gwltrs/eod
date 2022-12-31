module Main where

import Prelude

import Control.Apply ((*>))
import Control.Monad.Except (ExceptT(..), runExceptT)
import Data.Array (length)
import Data.Bifunctor (bimap)
import Data.Date (Date)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Slice (slice)
import Effect (Effect)
import Effect.Aff (Aff, Error, launchAff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Forceable (frc)
import IO (getBulkDays, getEODDays, getLiveDay, logAffE)
--import Indicators (atr, longFullness)
import Railroad (fuse, launchAffE)
import Type.Alias (AffE)
import Type.EODDay (toLiveDay)
import Type.YMD (YMD(..), ymd)
import Utils (slastN, slastN', (<<#>>))

previousTradingDate :: YMD
previousTradingDate = frc $ ymd 2022 12 23

main ∷ Effect Unit
main = getBulkDays previousTradingDate <#> length # logAffE # launchAffE
--main = getEODDays (frc $ ymd 2022 1 1) "BABA" <<#>> toLiveDay <#> (sLastN' 30 >>> atr) # logAffE # launchAffE