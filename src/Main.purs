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
import IO (findStocks, getBulkDays, getEODDays, getLiveDay, logAffE)
import Indicators (convex, day)
import Railroad (fuse, launchAffE)
import Type.Alias (AffE)
import Type.EODDay (toLiveDay)
import Type.Indicator (Indicator, (<<))
import Type.LiveDay (avg)
import Type.YMD (YMD(..), ymd)
import Utils (slastN, slastN', (<<#>>))

fromDate :: YMD
fromDate = frc $ ymd 2022 12 2

toDate :: YMD
toDate = frc $ ymd 2023 1 11

filter :: Indicator Boolean
filter = 
  let 
    isConvex = (_ >= 4) <$> (convex avg)
    today = avg <$> day
    yesterday = avg <$> day << 1
    twoDaysAgo = avg <$> day << 2
    todayAboveYesterday = (>) <$> today <*> yesterday
    twoDaysAgoAboveYesterday = (>) <$> twoDaysAgo <*> yesterday
    yesterdayIsLowest = (&&) <$> todayAboveYesterday <*> twoDaysAgoAboveYesterday
  in
    (&&) <$> isConvex <*> yesterdayIsLowest

main ∷ Effect Unit
main = launchAffE $ findStocks fromDate toDate ((_ >= 6) <$> (convex avg))