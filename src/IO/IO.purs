module IO where

import Prelude

import Control.Monad.Except (ExceptT, except)
import Data.Date (Date)
import Data.Maybe (Maybe)
import Effect.Aff (Aff, error)
import Effect.Class (liftEffect)
import IO.Atom (getAPIKey, getURL)
import Railroad (liftEffectE, toRight)
import Type.Alias (AffE, Sym, AffE)
import Type.BulkDay (BulkDay, bulkDaysFromJSON)
import Type.EODDay (EODDay, eodDaysFromJSON)
import Type.LiveDay (LiveDay, liveDayFromJSON)
import URL (bulkURL, eodURL, liveURL)

getBulkDays :: Date -> AffE (Array BulkDay)
getBulkDays date = do
  key <- liftEffectE getAPIKey
  res <- getURL (bulkURL key date)
  except $ toRight (error "Failed to parse bulk days JSON") $ bulkDaysFromJSON res

getEODDays :: Date -> Sym -> AffE (Array EODDay)
getEODDays date sym = do
  key <- liftEffectE getAPIKey
  res <- getURL (eodURL key date sym)
  except $ toRight (error "Failed to parse eod days JSON") $ eodDaysFromJSON res

getLiveDay :: Sym -> AffE LiveDay
getLiveDay sym = do
  key <- liftEffectE getAPIKey
  res <- getURL (liveURL key sym)
  except $ toRight (error "Failed to parse live day JSON") $ liveDayFromJSON res