module Test.Type.EODDay where

import Prelude

import Control.Monad.Free (Free)
import Data.Maybe (Maybe(..))
import Test.Unit (suite, test, TestF)
import Test.Unit.Assert as Assert
import Test.Value (aaplAmznBulkDays, aaplAmznBulkDaysJSON, aaplAmznEODDays, aaplBulkDay)
import Type.EODDay (eodDaysFromJSON, eodDaysToJSON, toDay)

eodDayTests :: Free TestF Unit
eodDayTests = suite "EODDay" do
  test "eodDaysFromJSON" do
    Assert.equal Nothing (eodDaysFromJSON "")
    Assert.equal (Just aaplAmznEODDays) (eodDaysFromJSON aaplAmznBulkDaysJSON)
  test "eodDaysToJSON" do
    Assert.equal (Just aaplAmznEODDays) (eodDaysFromJSON $ eodDaysToJSON aaplAmznEODDays)