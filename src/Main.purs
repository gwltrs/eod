module Main
  ( counterWidget
  , evaluator
  , fromDate
  , indicator
  , sqnAnalysis
  , toDate
  )
  where

import NestedApplicative
import Prelude

import AffEs (findHistory, findToday, getBulkDays, getEODDays, getLiveDay, logAffE, analyzeHistory, analyzeHistories)
import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P
import Concur.React.Run (runWidgetInDom)
import Control.Apply ((*>), lift2)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Data.Array (length)
import Data.Bifunctor (bimap)
import Data.Date (Date)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isJust)
import Data.Ord (lessThan)
import Data.Slice (Slice, slice)
import Data.String.Unsafe (charAt)
import Effect (Effect)
import Effect.Aff (Aff, Error, launchAff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Evaluators (maxPreviousLow)
import Forceable (frc)
import Indicators (convex, at, fibChunks, bullishReverse)
import SystemQuality (systemQuality, SQN)
import Type.AffE as AE
import Type.Alias (RMultiple)
import Type.Analysis (Analysis(..))
import Type.Day (Day, avg, close, high, low, open)
import Type.Evaluator (Evaluator(..))
import Type.Indicator (Indicator, last)
import Type.Purchase (Purchase, mkPurchase)
import Type.YMD (YMD(..), ymd)
import Utils (slastN, slastN', filterMaybe, bToMU, qualify, undefined)

fromDate :: YMD
fromDate = frc $ ymd 2022 1 1

toDate :: YMD
toDate = frc $ ymd 2023 4 17

indicator :: Indicator (Maybe Purchase)
indicator = pure Nothing
  
evaluator :: Evaluator (Purchase -> Number)
evaluator = maxPreviousLow 10

sqnAnalysis :: Analysis Purchase RMultiple SQN
sqnAnalysis = Analysis indicator evaluator systemQuality

counterWidget :: forall a. Int -> Widget HTML a
counterWidget count = do
  n <- D.div'
        [ D.p' [D.text ("State: " <> show count)]
        , D.button [P.onClick] [D.text "Increment"] $> count+1
        , D.button [P.onClick] [D.text "Decrement"] $> count-1
        ]
  liftEffect (log ("COUNT IS NOW: " <> show n))
  counterWidget n

main ∷ Effect Unit
main = runWidgetInDom "root" (counterWidget 0)
  --AE.launch (liftEffect <<< log <<< show) $ findToday fromDate toDate (isJust <$> indicator)
  -- AE.launch (liftEffect <<< log <<< show) $ findHistory "spxu" indicator
  --analyzeHistories (const true) sqnAnalysis <#> (\sqn -> "System quality number: " <> show sqn) >>= (AE.liftEffect <<< log) # AE.launch (liftEffect <<< log <<< show)