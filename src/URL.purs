module URL (bulkURL, eodURL, liveURL) where

import Prelude

import Data.Date (Date, year, month, day)
import Data.Enum (fromEnum)
import Type.Alias (APIKey, URL, Ticker)
import Type.YMD (YMD(..))

bulkURL :: APIKey -> YMD -> URL
bulkURL key date = baseURL <> "/eod-bulk-last-day/US?api_token=" <> key <> "&fmt=json&date=" <> show date

eodURL :: APIKey -> YMD -> Ticker -> URL
eodURL key date sym = baseURL <> "/eod/" <> sym <> ".US?api_token=" <> key <> "&fmt=json&from=" <> show date

liveURL :: APIKey -> Ticker -> URL
liveURL key sym = baseURL <> "/real-time/" <> sym <> ".US?api_token=" <> key <> "&fmt=json"

baseURL :: URL
baseURL = "https://eodhistoricaldata.com/api"