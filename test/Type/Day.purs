module Test.Type.Day where

import Prelude

import Control.Monad.Free (Free)
import Data.Maybe (Maybe(..))
import Test.Unit (suite, test, TestF)
import Test.Unit.Assert as Assert
import Test.Value (aaplAmznBulkDays, aaplAmznBulkDaysJSON, aaplBulkDayJSON, aaplBulkDay)
import Type.BulkDay (toEODDay)
import Type.EODDay (eodDaysFromJSON, toDay)
import Type.Day (dayFromJSON, day)

dayTests :: Free TestF Unit
dayTests = suite "Day" do
  test "dayFromJSON" do
    Assert.equal Nothing (dayFromJSON "")
    Assert.equal (Just $ toDay $ toEODDay aaplBulkDay) (dayFromJSON aaplBulkDayJSON)
  test "append" do
    Assert.equal (day 2.0 5.0 0.5 2.0 15.0) ((day 2.0 3.0 1.0 3.5 10.0) <> (day 5.0 5.0 0.5 2.0 5.0))
    Assert.equal (day 5.0 5.0 0.5 3.5 15.0) ((day 5.0 5.0 0.5 2.0 5.0) <> (day 2.0 3.0 1.0 3.5 10.0))