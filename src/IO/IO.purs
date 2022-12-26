module IO where

import Prelude

import Control.Monad.Except (ExceptT(..), except, runExceptT)
import Data.Array (filter)
import Data.Date (Date)
import Data.Either (Either(..))
import Data.Functor (voidLeft, voidRight)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple)
import Effect.Aff (Aff, error)
import Effect.Class (liftEffect)
import IO.Atom (getAPIKey, getURL)
import Railroad (liftEffectE, toRight)
import Type.Alias (AffE, Sym)
import Type.BulkDay (BulkDay, bulkDaysFromJSON, isOptimalBulkDay)
import Type.EODDay (EODDay, eodDaysFromJSON)
import Type.LiveDay (LiveDay, liveDayFromJSON)
import Type.YMD (YMD(..))
import URL (bulkURL, eodURL, liveURL)

getBulkDays :: YMD -> AffE (Array BulkDay)
getBulkDays date = do
  key <- liftEffectE getAPIKey
  res <- getURL (bulkURL key date)
  except $ toRight (error "Failed to parse bulk days JSON") $ filter isOptimalBulkDay <$> bulkDaysFromJSON res

--readBulkDays :: Date -> AffE (Array BulkDay)
--readBulkDays date = 

--writeBulkDays :: Date -> Array BulkDay -> AffE Unit
--writeBulkDays date arr = 

getEODDays :: YMD -> Sym -> AffE (Array EODDay)
getEODDays date sym = do
  key <- liftEffectE getAPIKey
  res <- getURL (eodURL key date sym)
  except $ toRight (error "Failed to parse eod days JSON") $ eodDaysFromJSON res

getLiveDay :: Sym -> AffE LiveDay
getLiveDay sym = do
  key <- liftEffectE getAPIKey
  res <- getURL (liveURL key sym)
  except $ toRight (error "Failed to parse live day JSON") $ liveDayFromJSON res

cacheAffE :: forall a b.  (a -> AffE b) -> (a -> b -> AffE Unit) -> (a -> AffE b) -> (a -> AffE b)
cacheAffE getCache setCache getData = (\a -> 
  runExceptT (getCache a) >>= (\e ->
    case e of
      Left _ -> getData a >>= (\d -> (setCache a d) *> (pure d)) # runExceptT
      _ -> pure e ) # ExceptT )