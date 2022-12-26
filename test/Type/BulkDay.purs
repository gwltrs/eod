module Test.Type.BulkDay where

import Prelude

import Control.Monad.Free (Free)
import Data.Maybe (Maybe(..))
import Test.Unit (suite, test, TestF)
import Test.Unit.Assert as Assert
import Test.Value (aaplAmznBulkDays, aaplAmznBulkDaysJSON, aaplBulkDay)
import Type.BulkDay (bulkDaysFromJSON, bulkDaysToJSON, isOptimalBulkDay)

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