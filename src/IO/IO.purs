module IO where

import Prelude

import Class.RandomAccess (rAt, rLen)
import Control.Monad.Except (ExceptT(..), catchError, except, runExceptT)
import Data.Array (fold, filter, snoc, sortWith, take, reverse)
import Data.Bifunctor (bimap)
import Data.Date (Date)
import Data.DateTime.Instant (fromDate)
import Data.Either (Either(..))
import Data.Functor (voidLeft, voidRight)
import Data.JSDate (toDate)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Number (remainder)
import Data.Slice (Slice, shead, slast, slen, slice, stail, stake)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), error)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Forceable (frc)
import IO.Atom (getTextFromURL)
import Prim.Boolean (True)
import Railroad (fuse, toRight)
import Type.Alias (Ticker)
import Type.Analysis (Analysis(..))
import Type.JSON.BulkDay (BulkDay(..), bulkDaysFromJSON, isOptimalBulkDay)
import Type.JSON.EODDay (EODDay, eodDaysFromJSON)
import Type.JSON.LiveDay (liveDayFromJSON)
import Type.Indicator (Indicator, indicate, indicate', minIndInputLength)
import Type.Day (Day, date, day, fromBulkDay, fromLiveDay, fromEODDay, ticker)
import Type.YMD (YMD(..), ymd)
import URL (bulkURL, eodURL, liveURL)
import Utils (slices', stail', filterMap, slastN, slastN', undefined)
import NestedApplicative
import Type.Evaluator (Evaluator(..), evaluate, evaluate', minEvalInputLength)
import Type.Analysis (Analysis(..), minAnalysisInputLength)
import Data.Foldable as DF
import Debug (spy)
import Effect.Now (nowDate)
import Effects (getToday)
import Affs (delaySeconds)
import Type.AffE (AffE)
import Type.AffE as AE
import EffectEs (getAPIKey)
import Type.AppError (AppError(..))

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

--recentTradingDate :: AffE YMD
--recentTradingDate = 

logAffE :: forall a. Show a => AffE a -> AffE a
logAffE a = 
  let logged = (runExceptT a) <#> bimap show show <#> fuse >>= (log >>> liftEffect) <#> Right # ExceptT
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