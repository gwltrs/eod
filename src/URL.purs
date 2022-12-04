module URL (bulkURL, eodURL, liveURL) where

import Prelude

import Data.Date (Date(..), year, month, day)
import Data.Enum (fromEnum)
import Data.String (length)
import Type.Alias (APIKey, Sym, URL)

bulkURL :: APIKey -> Date -> URL
bulkURL key date = baseURL <> "/eod-bulk-last-day/US?api_token=" <> key <> "&fmt=json&date=" <> dateStr date

eodURL :: APIKey -> Date -> Sym -> URL
eodURL key date sym = baseURL <> "/eod/" <> sym <> ".US?api_token=" <> key <> "&fmt=json&from=" <> dateStr date

liveURL :: APIKey -> Sym -> URL
liveURL key sym = baseURL <> "/real-time/" <> sym <> ".US?api_token=" <> key <> "&fmt=json"

baseURL :: URL
baseURL = "https://eodhistoricaldata.com/api"

dateStr :: Date -> String
dateStr date = 
  let 
    padZeros n s = if length s < n then padZeros n ("0" <> s) else s
    y = year date # fromEnum # show # padZeros 4
    m = month date # fromEnum # show # padZeros 2
    d = day date # fromEnum # show # padZeros 2
  in y <> "-" <> m <> "-" <> d