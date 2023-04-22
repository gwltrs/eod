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
import AffEs (findHistory, findToday, getBulkDays, getEODDays, getLiveDay, logAffE, analyzeHistory, analyzeHistories)
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
import Private (vcp)

fromDate :: YMD
fromDate = frc $ ymd 2022 1 1

toDate :: YMD
toDate = frc $ ymd 2023 4 17

indicator :: Indicator (Maybe Purchase)
indicator = vcp
  
evaluator :: Evaluator (Purchase -> Number)
evaluator = maxPreviousLow 10

sqnAnalysis :: Analysis Purchase RMultiple SQN
sqnAnalysis = Analysis indicator evaluator systemQuality

main ∷ Effect Unit
main = 
  --pure unit
  AE.launch (liftEffect <<< log <<< show) $ findToday fromDate toDate (isJust <$> indicator)
  --AE.launch (liftEffect <<< log <<< show) $ findHistory "spy" indicator
  --analyzeHistories (const true) sqnAnalysis <#> (\sqn -> "System quality number: " <> show sqn) >>= (AE.liftEffect <<< log) # AE.launch (liftEffect <<< log <<< show)