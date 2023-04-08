module AffEs where

import Prelude
import Data.Tuple (Tuple(..), fst, snd)
import Type.AffE (AffE)
import Type.AffE as AE
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, writeTextFile)
import Type.AppError (AppError(..))
import Fetch (fetch)
import Type.Alias (URL, Ticker)
import Type.YMD (YMD, ymd)
import Type.Day (Day)
import NestedApplicative
import Effect.Class (liftEffect)
import Effects (getToday)
import EffectEs (getAPIKey)
import URL (bulkURL, eodURL, liveURL)
import Type.Day (Day, date, day, fromBulkDay, fromLiveDay, fromEODDay, ticker)
import Data.Array (fold, filter, snoc, sortWith, take, reverse)
import Data.Either (Either(..), either)
import Type.JSON.BulkDay (BulkDay(..), bulkDaysFromJSON, isOptimalBulkDay)
import Type.JSON.EODDay (EODDay, eodDaysFromJSON)
import Type.JSON.LiveDay (liveDayFromJSON)
import Type.Evaluator (Evaluator(..), evaluate, evaluate', minEvalInputLength)
import Type.Analysis (Analysis(..), minAnalysisInputLength)
import Type.Indicator (Indicator, indicate, indicate', minIndInputLength)
import Forceable (frc)
import Data.Slice (Slice, shead, slast, slen, slice, stail, stake)
import Utils (slices', stail', filterMap, slastN, slastN', undefined)
import Affs (delaySeconds)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Control.Monad.Except (ExceptT(..), catchError, except, runExceptT)
import Effect.Console (log)
import Data.Traversable (sequence)
import Debug (spy)

readTextFromFile :: String -> AffE String
readTextFromFile = AE.tryAff FileError <<< readTextFile UTF8

writeTextToFile :: String -> String -> AffE Unit
writeTextToFile path text = AE.tryAff FileError $ writeTextFile UTF8 path text

getTextFromURL :: URL -> AffE String
getTextFromURL url = AE.tryAff NetworkError (fetch url {} >>= (\r -> r.text))

getBulkDays :: YMD -> AffE (Array Day)
getBulkDays date = do
  key <- AE.liftEffectE getAPIKey
  res <- getTextFromURL $ bulkURL key date
  AE.liftMaybe JSONParseError $ fromBulkDay <<$>> sortWith _.code <$> filter isOptimalBulkDay <$> bulkDaysFromJSON res

getTickers :: YMD -> AffE (Array Ticker)
getTickers date = ticker <<$>> getBulkDays date

getEODDays :: YMD -> Ticker -> AffE (Array Day)
getEODDays date sym = do
  key <- AE.liftEffectE getAPIKey
  res <- getTextFromURL $ eodURL key date sym
  AE.liftMaybe JSONParseError $ (fromEODDay sym) <<$>> eodDaysFromJSON res

getLiveDay :: Ticker -> AffE Day
getLiveDay sym = do
  today :: YMD <- liftEffect getToday
  key <- AE.liftEffectE getAPIKey
  res <- getTextFromURL $ liveURL key sym
  AE.liftMaybe JSONParseError $ (fromLiveDay sym today) <$> liveDayFromJSON res

getEODDaysAndLiveDay :: YMD -> Ticker -> AffE (Array Day)
getEODDaysAndLiveDay date sym = do
  eodDays <- getEODDays date sym
  --log' $ show $ slastN' 3 eodDays
  liveDay :: Day <- getLiveDay sym
  pure $ (eodDays <> [liveDay])

logAffE :: forall a. Show a => AffE a -> AffE a
logAffE a = 
  let logged = (runExceptT a) <#> either show show >>= (log >>> liftEffect) <#> Right # ExceptT
  in logged *> a

processHistory :: forall a b c. Ticker -> Analysis a b c -> AffE (Array b)
processHistory ticker analysis = 
  let (Analysis ind eval _) = analysis
  in do
    days <- getEODDays (frc $ ymd 1900 1 1) ticker
    slices' (minAnalysisInputLength analysis) days
      # filterMap (\s ->
        let
          i = join $ indicate ind (stake (minIndInputLength ind) s)
          e = frc $ evaluate eval (slastN (minEvalInputLength eval) s)
        in
          e <$> i)
      # pure

analyzeHistory :: forall a b c. Ticker -> Analysis a b c -> AffE c
analyzeHistory ticker analysis = 
  let (Analysis _ _ finally) = analysis
  in finally <$> processHistory ticker analysis

analyzeHistories :: forall a b c. (Ticker -> Boolean) -> Analysis a b c -> AffE c
analyzeHistories tickerFilter analysis = 
  let (Analysis ind eval finally) = analysis
  in do
    tickers :: Array String <- getTickers (frc $ ymd 2023 3 31)
      <#> filter tickerFilter
    processed :: Array (Array b) <- sequence $ (\t -> processHistory t analysis) <$> (spy "tickers" tickers)
    pure $ finally $ join processed

findHistory :: forall a. Ord a => Show a => Ticker -> Indicator (Maybe a) -> AffE Unit
findHistory ticker indicator = do
  days :: Array Day <- getEODDays (frc $ ymd 1900 1 1) ticker
  slices' (minIndInputLength indicator) days
    # filterMap (\s -> 
      let
        ind = join $ indicate indicator s
        date_ = show $ date $ frc $ slast s
      in
        ind <#> (\i -> Tuple i date_))
    # sortWith fst
    # reverse
    # map (show >>> log)
    # fold
    # liftEffect

findToday :: YMD -> YMD -> Indicator Boolean -> AffE Unit
findToday fromDate toDate isGoodPick = 
  let 
    traverseStocks :: Slice String -> Array String -> AffE (Array String)
    traverseStocks remaining matches = 
      if slen remaining == 0 then pure matches
      else do
        AE.liftAff $ delaySeconds 0.1
        days <- getEODDaysAndLiveDay fromDate (frc remaining)
          `catchError` (const $ pure [])
        case indicate' isGoodPick days of 
          Nothing -> do
            AE.liftEffect $ log "network error/insufficient number of days"
            traverseStocks (stail' remaining) matches
          (Just true) -> do
            AE.liftEffect $ log $ frc remaining
            AE.liftEffect $ log $ show $ slastN' 3 days
            traverseStocks (stail' remaining) (snoc matches (frc remaining))
          (Just false) -> do
            traverseStocks (stail' remaining) matches
  in do
    syms <- getBulkDays toDate <<#>> ticker
    picks <- traverseStocks (slice syms) []
    AE.liftEffect $ log $ show picks