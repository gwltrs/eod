module Main where

import Prelude

import Control.Apply ((*>), lift2)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Data.Array (length)
import Data.Bifunctor (bimap)
import Data.Date (Date)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isJust)
import Data.Ord (lessThan)
import Data.Slice (Slice, slice)
import Effect (Effect)
import Effect.Aff (Aff, Error, launchAff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Forceable (frc)
import IO (findHistory, findToday, getBulkDays, getEODDays, getLiveDay, logAffE, log', analyzeHistory)
import Railroad (fuse, launchAffE, launchAffE)
import Type.Alias (RMultiple)
import Type.EODDay (toDay)
import Type.Indicator (Indicator, last)
import Indicators (convex, at, fibChunks, bullishReverse)
import Type.Day (Day, avg, close, high, low, open)
import Type.YMD (YMD(..), ymd)
import Utils (slastN, slastN', filterMaybe, bToMU, qualify, undefined)
import NestedApplicative
import Type.Analysis (Analysis(..))
import Evaluators (maxPreviousLow)
import Type.Evaluator (Evaluator(..))
import Type.Purchase (Purchase(..))
import SystemQuality (systemQuality, SQN)

fromDate :: YMD
fromDate = frc $ ymd 2022 1 1

toDate :: YMD
toDate = frc $ ymd 2023 3 6

mkPurchase :: Number -> Number -> Purchase
mkPurchase buy stop = Purchase { buyPrice: buy, stopPrice: stop }

indicator :: Indicator (Maybe Purchase)
indicator = 
  let 
    chunks = low <<$>> fibChunks <$> last 88
    reversed = bullishReverse <$> chunks
    convexStreak = convex <$> chunks
    streakLongEnough = (_ >= 7) <$> convexStreak
    isUpDay = let d = at 0 in lift2 (>) (close <$> d) (open <$> d) 
    purchase = mkPurchase <$> (close <$> at 0) <*> (low <$> at 0)
  in
    qualify [reversed, streakLongEnough, isUpDay] purchase

evaluator :: Evaluator (Purchase -> Number)
evaluator = maxPreviousLow

sqnAnalysis :: Analysis Purchase RMultiple SQN
sqnAnalysis = Analysis indicator evaluator systemQuality

main ∷ Effect Unit
main = 
  --pure unit
  --launchAffE $ findToday fromDate toDate (isJust <$> indicator)
  --launchAffE $ findHistory "atos" indicator
  analyzeHistory "ATOS" sqnAnalysis
    <#> (\sqn -> "System quality number: " <> show sqn)
    >>= log'
    # launchAffE