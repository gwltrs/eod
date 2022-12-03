module Test.Type.EODDay where

import Prelude

import Control.Monad.Free (Free)
import Data.Maybe (Maybe(..))
import Test.Unit (suite, test, TestF)
import Test.Unit.Assert as Assert
import Test.Value (aaplAmznBulkDays, aaplAmznBulkDaysJSON)
import Type.BulkDay (toEODDay)
import Type.EODDay (eodDaysFromJSON)

eodDayTests :: Free TestF Unit
eodDayTests = suite "EODDay" do
  test "eodDaysFromJSON" do
    Assert.equal Nothing (eodDaysFromJSON "")
    Assert.equal (aaplAmznBulkDays <#> toEODDay # Just) (eodDaysFromJSON aaplAmznBulkDaysJSON)