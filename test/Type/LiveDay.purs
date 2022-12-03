module Test.Type.LiveDay where

import Prelude

import Control.Monad.Free (Free)
import Data.Maybe (Maybe(..))
import Test.Unit (suite, test, TestF)
import Test.Unit.Assert as Assert
import Test.Value (aaplAmznBulkDays, aaplAmznBulkDaysJSON, aaplBulkDayJSON, aaplBulkDay)
import Type.BulkDay (toEODDay)
import Type.EODDay (eodDaysFromJSON, toLiveDay)
import Type.LiveDay (liveDayFromJSON)

liveDayTests :: Free TestF Unit
liveDayTests = suite "LiveDay" do
  test "liveDayFromJSON" do
    Assert.equal Nothing (liveDayFromJSON "")
    Assert.equal (Just $ toLiveDay $ toEODDay aaplBulkDay) (liveDayFromJSON aaplBulkDayJSON)