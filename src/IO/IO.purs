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
import Type.BulkDay (BulkDay, bulkDaysFromJSON, isOptimalBulkDay)
import Type.EODDay (EODDay, eodDaysFromJSON, toDay)
import Type.Indicator (Indicator, indicate, indicate', minIndInputLength)
import Type.Day (Day, day, dayFromJSON)
import Type.YMD (YMD(..), ymd)
import URL (bulkURL, eodURL, liveURL)
import Utils (slices', stail', filterMap, slastN, slastN', undefined)
import NestedApplicative
import Type.Evaluator (Evaluator(..), evaluate, evaluate', minEvalInputLength)
import Type.BacktestResult (BacktestResult(..))
import Type.Analysis (Analysis(..), minAnalysisInputLength)
import Data.Foldable as DF

getBulkDays :: YMD -> AffE (Array BulkDay)
getBulkDays date = do
  key <- liftEffectE getAPIKey
  res <- getURL $ bulkURL key date
  except $ toRight (error "Failed to parse bulk days JSON") $ sortWith _.code <$> filter isOptimalBulkDay <$> bulkDaysFromJSON res

--readBulkDays :: Date -> AffE (Array BulkDay)
--readBulkDays date = 

--writeBulkDays :: Date -> Array BulkDay -> AffE Unit
--writeBulkDays date arr = 

getEODDays :: YMD -> Ticker -> AffE (Array EODDay)
getEODDays date sym = do
  key <- liftEffectE getAPIKey
  res <- getURL $ eodURL key date sym
  except $ toRight (error "Failed to parse eod days JSON") $ eodDaysFromJSON res

getLiveDay :: Ticker -> AffE Day
getLiveDay sym = do
  key <- liftEffectE getAPIKey
  res <- getURL $ liveURL key sym
  except $ toRight (error "Failed to parse live day JSON") $ dayFromJSON res

getEODDaysAndLiveDay :: YMD -> Ticker -> AffE (Array Day)
getEODDaysAndLiveDay date sym = do
  eodDays <- getEODDays date sym
  --log' $ show $ slastN' 3 eodDays
  liveDay <- getLiveDay sym
  pure $ (toDay <$> eodDays) <> [liveDay]

cacheAffE :: forall a b.  (a -> AffE b) -> (a -> b -> AffE Unit) -> (a -> AffE b) -> (a -> AffE b)
cacheAffE getCache setCache getData = (\a -> 
  runExceptT (getCache a) >>= (\e ->
    case e of
      Left _ -> getData a >>= (\d -> (setCache a d) *> (pure d)) # runExceptT
      _ -> pure e ) # ExceptT )

logAffE :: forall a. Show a => AffE a -> AffE a
logAffE a = 
  let logged = (runExceptT a) <#> bimap show show <#> fuse >>= (log >>> liftEffect) <#> Right # ExceptT
  in logged *> a

delayE :: Number -> AffE Unit
delayE seconds = ExceptT (Right <$> (delay $ Milliseconds $ seconds * 1000.0))

log' :: String -> AffE Unit
log' = log >>> toAffE

--backtestHistory :: forall a b c. Ticker -> Indicator (Maybe a) -> Evaluator (a -> Number) -> AffE (Array BacktestResult) 
--backtestHistory ticker analysis accum = do
--  days <- getEODDays (frc $ ymd 1900 1 1) ticker
--  slices' (minAnalysisInputLength analysis) days

analyzeHistory :: forall a b c. Monoid b => Ticker -> Analysis a b c -> AffE c
analyzeHistory ticker analysis = 
  let (Analysis ind eval finally) = analysis
  in do
    days <- getEODDays (frc $ ymd 1900 1 1) ticker
    slices' (minAnalysisInputLength analysis) (map toDay days)
      # filterMap (\s ->
        let
          i = join $ indicate ind (stake (minIndInputLength ind) s)
          e = frc $ evaluate eval (slastN (minEvalInputLength eval) s)
        in
          e <$> i)
      # fold
      # finally
      # pure

findHistory :: forall a. Ord a => Show a => Ticker -> Indicator (Maybe a) -> AffE Unit
findHistory ticker indicator = do
  days <- getEODDays (frc $ ymd 1900 1 1) ticker
  slices' (minIndInputLength indicator) days
    # filterMap (\s -> 
      let
        ind = join $ indicate indicator $ map toDay s
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