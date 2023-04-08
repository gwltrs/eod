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
import IO (findHistory, findToday, getBulkDays, getEODDays, getLiveDay, logAffE, analyzeHistory, analyzeHistories)
import Type.Alias (RMultiple)
import Type.Indicator (Indicator, last)
import Indicators (convex, at, fibChunks, bullishReverse)
import Type.Day (Day, avg, close, high, low, open)
import Type.YMD (YMD(..), ymd)
import Utils (slastN, slastN', filterMaybe, bToMU, qualify, undefined)
import NestedApplicative
import Type.Analysis (Analysis(..))
import Evaluators (maxPreviousLow)
import Type.Evaluator (Evaluator(..))
import Type.Purchase (Purchase, mkPurchase)
import SystemQuality (systemQuality, SQN)
import Data.String.Unsafe (charAt)
import Type.AffE as AE

fromDate :: YMD
fromDate = frc $ ymd 2022 1 1

toDate :: YMD
toDate = frc $ ymd 2023 3 6

indicator :: Indicator (Maybe Purchase)
indicator = 
  let 
    chunks = low <<$>> fibChunks <$> last 88
    reversed = bullishReverse <$> chunks
    convexStreak = convex <$> chunks
    streakLongEnough = (_ >= 7) <$> convexStreak
    isUpDay = let d = at 0 in lift2 (>) (close <$> d) (open <$> d) 
    priority = pure 0.0
    purchase = (\a b c -> frc $ mkPurchase a b c) <$> (close <$> at 0) <*> (low <$> at 0) <*> priority
  in
    qualify [reversed, streakLongEnough, isUpDay] purchase

evaluator :: Evaluator (Purchase -> Number)
evaluator = maxPreviousLow 10

sqnAnalysis :: Analysis Purchase RMultiple SQN
sqnAnalysis = Analysis indicator evaluator systemQuality

main ∷ Effect Unit
main = 
  --pure unit
  --launchAffE $ findToday fromDate toDate (isJust <$> indicator)
  --launchAffE $ findHistory "atos" indicator
  analyzeHistories (\t -> charAt 0 t == 'Z') sqnAnalysis
    <#> (\sqn -> "System quality number: " <> show sqn)
    >>= (AE.liftEffect <<< log)
    # AE.launch (liftEffect <<< log <<< show)