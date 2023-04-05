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
import Effect.Aff (Aff, Milliseconds(..), delay, error)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Forceable (frc)
import IO.Atom (getAPIKey, getURL)
import Prim.Boolean (True)
import Railroad (fuse, liftEffectE, toAffE, toRight)
import Type.Alias (AffE, Ticker)
import Type.Analysis (Analysis(..))
import Type.JSON.BulkDay (BulkDay(..), bulkDaysFromJSON, isOptimalBulkDay)
import Type.JSON.EODDay (EODDay, eodDaysFromJSON)
import Type.JSON.LiveDay (liveDayFromJSON)
import Type.Indicator (Indicator, indicate, indicate', minIndInputLength)
import Type.Day (Day, day, fromBulkDay, fromLiveDay, fromEODDay, ticker)
import Type.YMD (YMD(..), ymd)
import URL (bulkURL, eodURL, liveURL)
import Utils (slices', stail', filterMap, slastN, slastN', undefined)
import NestedApplicative
import Type.Evaluator (Evaluator(..), evaluate, evaluate', minEvalInputLength)
import Type.Analysis (Analysis(..), minAnalysisInputLength)
import Data.Foldable as DF
import Debug (spy)

getToday :: AffE YMD
getToday = pure $ frc $ ymd 2023 4 5

getBulkDays :: YMD -> AffE (Array Day)
getBulkDays date = do
  key <- liftEffectE getAPIKey
  res <- getURL $ bulkURL key date
  except $ toRight (error "Failed to parse bulk days JSON") $ fromBulkDay <<$>> sortWith _.code <$> filter isOptimalBulkDay <$> bulkDaysFromJSON res

getTickers :: YMD -> AffE (Array Ticker)
getTickers date = ticker <<$>> getBulkDays date

getEODDays :: YMD -> Ticker -> AffE (Array Day)
getEODDays date sym = do
  key <- liftEffectE getAPIKey
  res <- getURL $ eodURL key date sym
  except $ toRight (error "Failed to parse eod days JSON") $ (fromEODDay sym) <<$>> eodDaysFromJSON res

getLiveDay :: Ticker -> AffE Day
getLiveDay sym = do
  today :: YMD <- getToday
  key <- liftEffectE getAPIKey
  res <- getURL $ liveURL key sym
  except $ toRight (error "Failed to parse live day JSON") $ (fromLiveDay sym today) <$> liveDayFromJSON res

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

delayE :: Number -> AffE Unit
delayE seconds = ExceptT (Right <$> (delay $ Milliseconds $ seconds * 1000.0))

log' :: String -> AffE Unit
log' = log >>> toAffE

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
  days <- getEODDays (frc $ ymd 1900 1 1) ticker
  slices' (minIndInputLength indicator) days
    # filterMap (\s -> 
      let
        ind = join $ indicate indicator $ map (fromEODDay ticker) s
        date = show $ _.date $ frc $ slast s
      in
        ind <#> (\i -> Tuple i date))
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
        delayE 0.1
        days <- getEODDaysAndLiveDay fromDate (frc remaining)
          `catchError` (const $ pure [])
        case indicate' isGoodPick days of 
          Nothing -> do
            log' "network error/insufficient number of days"
            traverseStocks (stail' remaining) matches
          (Just true) -> do
            log' $ frc remaining
            log' $ show $ slastN' 3 days
            traverseStocks (stail' remaining) (snoc matches (frc remaining))
          (Just false) -> do
            traverseStocks (stail' remaining) matches
  in do
    syms <- getBulkDays toDate <<#>> _.code
    picks <- traverseStocks (slice syms) []
    toAffE $ log $ show picks