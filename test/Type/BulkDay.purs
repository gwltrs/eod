module Test.Type.BulkDay where

import Prelude

import Control.Monad.Free (Free)
import Data.Maybe (Maybe(..))
import Test.Unit (suite, test, TestF)
import Test.Unit.Assert as Assert
import Test.Value (aaplAmznBulkDays, aaplAmznBulkDaysJSON, aaplBulkDay)
import Type.BulkDay (bulkDaysFromJSON, isOptimalBulkDay)

bulkDayTests :: Free TestF Unit
bulkDayTests = suite "BulkDay" do
  test "bulkDaysFromJSON" do
    Assert.equal Nothing (bulkDaysFromJSON "")
    Assert.equal (Just aaplAmznBulkDays) (bulkDaysFromJSON aaplAmznBulkDaysJSON)
  -- test "bulkDayFromJSON" do
  --   Assert.equal Nothing (parseDay "")
  --   Assert.equal 
  --     (Just { date: "2022-12-01", open: 148.25, high: 149.125, low: 146.5, close: 148.0, volume: 68230295.0 })
  --     (parseDay """{"code":"AAPL","exchange_short_name":"US","date":"2022-12-01","open":148.25,"high":149.125,"low":146.50,"close":148,"adjusted_close":148.77,"volume":68230295}""")
  -- test "bulkDaysFromJSON" do
  --   Assert.equal 
  --     Nothing
  --     (parseDayWithCode "")
  --   Assert.equal 
  --     (Just { code: "AAPL", date: "2022-12-01", open: 148.25, high: 149.125, low: 146.5, close: 148.0, volume: 68230295.0 })
  --     (parseDayWithCode """{"code":"AAPL","exchange_short_name":"US","date":"2022-12-01","open":148.25,"high":149.125,"low":146.50,"close":148,"adjusted_close":148.77,"volume":68230295}""")
  test "isOptimalBulkDay" do
    Assert.equal true (isOptimalBulkDay aaplBulkDay)
    Assert.equal false (isOptimalBulkDay aaplBulkDay { code = "AAEKX" })
    Assert.equal false (isOptimalBulkDay aaplBulkDay { code = "AKO-A" })
    Assert.equal false (isOptimalBulkDay aaplBulkDay { close = 10.0, volume = 10.0 })
    Assert.equal false (isOptimalBulkDay aaplBulkDay { code = "LVOPX", volume = 0.0 })



-- networkingTests :: Free TestF Unit
-- networkingTests = suite "Networking" do
--   test "parseDay" do
--     Assert.equal 
--       Nothing
--       (parseDay "")
--     Assert.equal 
--       (Just { date: "2022-12-01", open: 148.25, high: 149.125, low: 146.5, close: 148.0, volume: 68230295.0 })
--       (parseDay """{"code":"AAPL","exchange_short_name":"US","date":"2022-12-01","open":148.25,"high":149.125,"low":146.50,"close":148,"adjusted_close":148.77,"volume":68230295}""")
--   test "parseDayWithCode" do
--     Assert.equal 
--       Nothing
--       (parseDayWithCode "")
--     Assert.equal 
--       (Just { code: "AAPL", date: "2022-12-01", open: 148.25, high: 149.125, low: 146.5, close: 148.0, volume: 68230295.0 })
--       (parseDayWithCode """{"code":"AAPL","exchange_short_name":"US","date":"2022-12-01","open":148.25,"high":149.125,"low":146.50,"close":148,"adjusted_close":148.77,"volume":68230295}""")
--   test "isOptimalBulkDay" do
--     Assert.equal 
--       true
--       (isOptimalBulkDay aaplDay)
--     Assert.equal 
--       false
--       (isOptimalBulkDay aaplDay { code = "AAEKX" })
--     Assert.equal 
--       false
--       (isOptimalBulkDay aaplDay { code = "AKO-A" })
--     Assert.equal 
--       false
--       (isOptimalBulkDay aaplDay { close = 10.0, volume = 10.0 })
--     Assert.equal 
--       false
--       (isOptimalBulkDay aaplDay { code = "LVOPX", volume = 0.0 })


