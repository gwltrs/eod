module Main where

import Prelude

import Control.Apply ((*>), lift2)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Data.Array (length)
import Data.Bifunctor (bimap)
import Data.Date (Date)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Ord (lessThan)
import Data.Slice (Slice, slice)
import Effect (Effect)
import Effect.Aff (Aff, Error, launchAff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Forceable (frc)
import IO (findHistory, findToday, getBulkDays, getEODDays, getLiveDay, logAffE)
import Indicators (convex, at)
import Railroad (fuse, launchAffE)
import Type.Alias (AffE)
import Type.EODDay (toDay)
import Type.Indicator (Indicator, last)
import Type.Day (Day, avg, close)
import Type.YMD (YMD(..), ymd)
import Utils (slastN, slastN', filterMaybe, bToMU)
import Nested

fromDate :: YMD
fromDate = frc $ ymd 2022 12 2

toDate :: YMD
toDate = frc $ ymd 2023 1 12

-- filter :: Indicator Boolean
-- filter = 
--   let 
--     isConvex = (_ >= 4) <$> (convex avg)
--     today = avg <$> day
--     yesterday = avg <$> day << 1
--     twoDaysAgo = avg <$> day << 2
--     todayAboveYesterday = (>) <$> today <*> yesterday
--     twoDaysAgoAboveYesterday = (>) <$> twoDaysAgo <*> yesterday
--     yesterdayIsLowest = (&&) <$> todayAboveYesterday <*> twoDaysAgoAboveYesterday
--   in
--     (&&) <$> isConvex <*> yesterdayIsLowest

indicator :: Indicator (Maybe Int)
indicator = 
  let 
    streak = ((filterMaybe (_ >= 4)) <<< Just <<< convex) <$> (avg <<$>> last 15)
    avgAt i = avg <$> at i
    reversed = lift2 lessThan (avgAt 1) (lift2 min (avgAt 0) (avgAt 2))
  in
    streak <<* (bToMU <$> reversed)


main ∷ Effect Unit
main = 
  --pure unit
  --launchAffE $ findToday fromDate toDate indicator
  launchAffE $ findHistory "ATOS" indicator