module Test.Value where

import Prelude

import Forceable (frc)
import Type.BulkDay (BulkDay, toEODDay)
import Type.EODDay (EODDay)
import Type.YMD (ymd)

aaplBulkDay :: BulkDay
aaplBulkDay = { code: "AAPL", date: frc $ ymd 2022 12 1, open: 148.25, high: 149.125, low: 146.50, close: 148.0, volume: 68230295.0 }

amznBulkDay :: BulkDay
amznBulkDay = { code: "AMZN", date: frc $ ymd 2022 12 2, open: 94.25, high: 95.25, low: 93.75, close: 94.0, volume: 72496388.0 }

aaplAmznBulkDays :: Array BulkDay
aaplAmznBulkDays = [aaplBulkDay, amznBulkDay]

aaplAmznEODDays :: Array EODDay
aaplAmznEODDays = [aaplBulkDay, amznBulkDay] <#> toEODDay

aaplBulkDayJSON :: String
aaplBulkDayJSON = """{"code":"AAPL","exchange_short_name":"US","date":"2022-12-01","open":148.25,"high":149.125,"low":146.50,"close":148,"adjusted_close":148.77,"volume":68230295}"""

aaplAmznBulkDaysJSON :: String
aaplAmznBulkDaysJSON = """[{"code":"AAPL","exchange_short_name":"US","date":"2022-12-01","open":148.25,"high":149.125,"low":146.50,"close":148,"adjusted_close":148.77,"volume":68230295},{"code":"AMZN","exchange_short_name":"US","date":"2022-12-02","open":94.25,"high":95.25,"low":93.75,"close":94.0,"adjusted_close":94.1,"volume":72496388}]"""