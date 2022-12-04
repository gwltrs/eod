module IO where

import Prelude

import Data.Date (Date)
import Data.Maybe (Maybe)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import IO.Atom (getAPIKey, getURL)
import Type.Alias (Sym)
import Type.BulkDay (BulkDay, bulkDaysFromJSON)
import Type.EODDay (EODDay, eodDaysFromJSON)
import Type.LiveDay (LiveDay, liveDayFromJSON)
import URL (bulkURL, eodURL, liveURL)

getBulkDays :: Date -> Aff (Maybe (Array BulkDay))
getBulkDays date = do
  key <- liftEffect getAPIKey
  getURL (bulkURL key date) <#> bulkDaysFromJSON

getEODDays :: Date -> Sym -> Aff (Maybe (Array EODDay))
getEODDays date sym = do
  key <- liftEffect getAPIKey
  getURL (eodURL key date sym) <#> eodDaysFromJSON

getLiveDay :: Sym -> Aff (Maybe LiveDay)
getLiveDay sym = do
  key <- liftEffect getAPIKey
  getURL (liveURL key sym) <#> liveDayFromJSON