module IO where

import Prelude

import Class.RandomAccess (rAt, rLen)
import Control.Monad.Except (ExceptT(..), catchError, except, runExceptT)
import Data.Array (filter, fold, snoc, sort, sortWith, take)
import Data.Bifunctor (bimap)
import Data.Date (Date)
import Data.DateTime.Instant (fromDate)
import Data.Either (Either(..))
import Data.Functor (voidLeft, voidRight)
import Data.JSDate (toDate)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (remainder)
import Data.Slice (Slice, shead, slast, slen, slice, stail)
import Data.Traversable (sequence)
import Data.Tuple (Tuple)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay, error)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Forceable (frc)
import IO.Atom (getAPIKey, getURL)
import Prim.Boolean (True)
import Railroad (fuse, liftEffectE, toAffE, toRight)
import Type.Alias (AffE, Ticker)
import Type.BulkDay (BulkDay, bulkDaysFromJSON, isOptimalBulkDay)
import Type.EODDay (EODDay, eodDaysFromJSON, toLiveDay)
import Type.Indicator (Indicator, indicate, indicate', minimumInputLength)
import Type.LiveDay (LiveDay, liveDay, liveDayFromJSON)
import Type.YMD (YMD(..), ymd)
import URL (bulkURL, eodURL, liveURL)
import Utils (slices', stail', (<<#>>))

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

getLiveDay :: Ticker -> AffE LiveDay
getLiveDay sym = do
  key <- liftEffectE getAPIKey
  res <- getURL $ liveURL key sym
  except $ toRight (error "Failed to parse live day JSON") $ liveDayFromJSON res

getEODDaysAndLiveDay :: YMD -> Ticker -> AffE (Array LiveDay)
getEODDaysAndLiveDay date sym = do
  eodDays <- getEODDays date sym
  liveDay <- getLiveDay sym
  pure $ (toLiveDay <$> eodDays) <> [liveDay]

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

logE :: String -> AffE Unit
logE = log >>> toAffE

data SearchType = Live | History Ticker

findHistory :: Ticker -> Indicator Boolean -> AffE Unit
findHistory ticker isGoodPick = do
  days <- getEODDays (frc $ ymd 1900 1 1) ticker
  slices' (minimumInputLength isGoodPick) days
    # filter ((_ <#> toLiveDay) >>> indicate isGoodPick >>> fromMaybe false)
    <#> (slast >>> frc >>> _.date >>> show >>> log) -- Array (Effect Unit)
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
            logE "network error/insufficient number of days"
            traverseStocks (stail' remaining) matches
          (Just true) -> do
            logE $ frc remaining
            traverseStocks (stail' remaining) (snoc matches (frc remaining))
          (Just false) -> do
            traverseStocks (stail' remaining) matches
  in do
    syms <- getBulkDays toDate <<#>> _.code
    picks <- traverseStocks (slice syms) []
    toAffE $ log $ show picks