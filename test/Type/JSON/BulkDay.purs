module Test.Type.JSON.BulkDay where

import Prelude

import Control.Monad.Free (Free)
import Data.Maybe (Maybe(..))
import Test.Unit (suite, test, TestF)
import Test.Unit.Assert as Assert
import Test.Value (aaplAmznBulkDays, aaplAmznBulkDaysJSON, aaplBulkDay, amznBulkDay)
import Type.JSON.BulkDay (BulkDay, bulkDay, bulkDaysFromJSON, bulkDaysToJSON, isOptimalBulkDay, toEODDay)
import Data.Newtype (unwrap)

bulkDayTests :: Free TestF Unit
bulkDayTests = suite "BulkDay" do
  test "bulkDaysFromJSON" do
    Assert.equal Nothing (bulkDaysFromJSON "")
    Assert.equal (Just aaplAmznBulkDays) (bulkDaysFromJSON aaplAmznBulkDaysJSON)
  test "bulkDaysToJSON" do
    Assert.equal (Just aaplAmznBulkDays) (bulkDaysFromJSON $ bulkDaysToJSON aaplAmznBulkDays)
  test "isOptimalBulkDay" do
    Assert.equal true (isOptimalBulkDay aaplBulkDay)
    Assert.equal false (isOptimalBulkDay aaplBulkDay { code = "AAEKX" })
    Assert.equal false (isOptimalBulkDay aaplBulkDay { code = "AKO-A" })
    Assert.equal false (isOptimalBulkDay aaplBulkDay { close = 10.0, volume = 10.0 })
    Assert.equal false (isOptimalBulkDay aaplBulkDay { code = "LVOPX", volume = 0.0 })
--  test "day conversions round-trip" do
--    Assert.equal amznBulkDay (roundTrip amznBulkDay)
--    Assert.equal aaplBulkDay (roundTrip aaplBulkDay)

--roundTrip :: BulkDay -> BulkDay
--roundTrip bd = 
--  let 
--    eod = toEODDay bd
--    live = unwrap $ toDay bd.code eod
--  in
--    bulkDay bd.code bd.date (live.open) (live.high) (live.low) (live.close) (live.volume)