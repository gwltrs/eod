module IO where

import Prelude

import Class.RandomAccess (rAt, rLen)
import Control.Monad.Except (ExceptT(..), catchError, except, runExceptT)
import Data.Array (filter, snoc, sort, sortWith, take)
import Data.Bifunctor (bimap)
import Data.Date (Date)
import Data.DateTime.Instant (fromDate)
import Data.Either (Either(..))
import Data.Functor (voidLeft, voidRight)
import Data.JSDate (toDate)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (remainder)
import Data.Slice (Slice, shead, slen, slice, stail)
import Data.Tuple (Tuple)
import Effect.Aff (Aff, Milliseconds(..), delay, error)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Forceable (frc)
import IO.Atom (getAPIKey, getURL)
import Prim.Boolean (True)
import Railroad (fuse, liftEffectE, toAffE, toRight)
import Type.Alias (Sym, AffE)
import Type.BulkDay (BulkDay, bulkDaysFromJSON, isOptimalBulkDay)
import Type.EODDay (EODDay, eodDaysFromJSON, toLiveDay)
import Type.Indicator (Indicator, indicate')
import Type.LiveDay (LiveDay, liveDay, liveDayFromJSON)
import Type.YMD (YMD(..))
import URL (bulkURL, eodURL, liveURL)
import Utils (stail', (<<#>>))

getBulkDays :: YMD -> AffE (Array BulkDay)
getBulkDays date = do
  key <- liftEffectE getAPIKey
  res <- getURL $ bulkURL key date
  except $ toRight (error "Failed to parse bulk days JSON") $ sortWith _.code <$> filter isOptimalBulkDay <$> bulkDaysFromJSON res

--readBulkDays :: Date -> AffE (Array BulkDay)
--readBulkDays date = 

--writeBulkDays :: Date -> Array BulkDay -> AffE Unit
--writeBulkDays date arr = 

getEODDays :: YMD -> Sym -> AffE (Array EODDay)
getEODDays date sym = do
  key <- liftEffectE getAPIKey
  res <- getURL $ eodURL key date sym
  except $ toRight (error "Failed to parse eod days JSON") $ eodDaysFromJSON res

getLiveDay :: Sym -> AffE LiveDay
getLiveDay sym = do
  key <- liftEffectE getAPIKey
  res <- getURL $ liveURL key sym
  except $ toRight (error "Failed to parse live day JSON") $ liveDayFromJSON res

getEODDaysAndLiveDay :: YMD -> Sym -> AffE (Array LiveDay)
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

findStocks :: YMD -> YMD -> Indicator Boolean -> AffE Unit
findStocks fromDate toDate filter = 
  let 
    traverseStocks :: Slice String -> Array String -> AffE (Array String)
    traverseStocks remaining matches = 
      if slen remaining == 0 then pure matches
      else do
        --logE $ frc remaining
        delayE 0.5
        -- if (rLen remaining >= 2) && ((rAt 0 remaining) /= (rAt 1 remaining)) then
        --   pure []
        -- else 
        --   pure []
        days <- getEODDaysAndLiveDay fromDate (frc remaining)
          --`catchError` (const $ pure [])
        case indicate' filter days of 
          Nothing -> do
            logE "network error/insufficient number of days"
            traverseStocks (stail' remaining) matches
          (Just true) -> do
            logE $ frc remaining
            traverseStocks (stail' remaining) (snoc matches (frc remaining))
          (Just false) -> do
            traverseStocks (stail' remaining) matches
  in do
    syms <- getBulkDays toDate <<#>> _.code -- <#> take 10
    picks <- traverseStocks (slice syms) []
    toAffE $ log $ show picks