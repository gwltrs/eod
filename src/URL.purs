module URL where

import Prelude

import Data.Date (Date)
import Type.Alias (APIKey, Sym, URL)

bulkURL :: APIKey -> Date -> URL
bulkURL key date = ""

eodURL :: APIKey -> URL
eodURL key = ""

liveURL :: APIKey -> Date -> Sym -> URL
liveURL key date sym = ""