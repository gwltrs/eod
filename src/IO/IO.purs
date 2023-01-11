module IO where

import Prelude

import Control.Monad.Except (ExceptT(..), except, runExceptT)
import Data.Array (filter)
import Data.Bifunctor (bimap)
import Data.Date (Date)
import Data.Either (Either(..))
import Data.Functor (voidLeft, voidRight)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple)
import Effect.Aff (Aff, error)
import Effect.Class (liftEffect)
import Effect.Console (log)
import IO.Atom (getAPIKey, getURL)
import Railroad (fuse, liftEffectE, toRight)
import Type.Alias (Sym, AffE)
import Type.BulkDay (BulkDay, bulkDaysFromJSON, isOptimalBulkDay)
import Type.EODDay (EODDay, eodDaysFromJSON, toLiveDay)
import Type.Indicator (Indicator)
import Type.LiveDay (LiveDay, liveDay, liveDayFromJSON)
import Type.YMD (YMD(..))
import URL (bulkURL, eodURL, liveURL)

getBulkDays :: YMD -> AffE (Array BulkDay)
getBulkDays date = do
  key <- liftEffectE getAPIKey
  res <- getURL $ bulkURL key date
  except $ toRight (error "Failed to parse bulk days JSON") $ filter isOptimalBulkDay <$> bulkDaysFromJSON res

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

getEODDaysWithLiveDay :: YMD -> Sym -> AffE (Array LiveDay)
getEODDaysWithLiveDay date sym = do
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

-- filterPrint :: Indicator Boolean -> AffE (Array String)
-- filterAndPrint filter = 
--   let inner