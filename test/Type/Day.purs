module Test.Type.Day where

import Prelude

import Control.Monad.Free (Free)
import Data.Maybe (Maybe(..))
import Test.Unit (suite, test, TestF)
import Test.Unit.Assert as Assert
import Test.Value (aaplAmznBulkDays, aaplAmznBulkDaysJSON, aaplBulkDayJSON, aaplBulkDay)
import Type.BulkDay (toEODDay)
import Type.EODDay (eodDaysFromJSON, toDay)
import Type.Day (dayFromJSON)

dayTests :: Free TestF Unit
dayTests = suite "Day" do
  test "dayFromJSON" do
    Assert.equal Nothing (dayFromJSON "")
    Assert.equal (Just $ toDay $ toEODDay aaplBulkDay) (dayFromJSON aaplBulkDayJSON)