module Test.Type.Day where

import Prelude

import Control.Monad.Free (Free)
import Data.Maybe (Maybe(..))
import Test.Unit (suite, test, TestF)
import Test.Unit.Assert as Assert
import Test.Value (aaplAmznBulkDays, aaplAmznBulkDaysJSON, aaplBulkDayJSON, aaplBulkDay)
import Type.JSON.BulkDay (toEODDay)
import Type.JSON.EODDay (eodDaysFromJSON)
import Type.Day (day)
import Type.YMD (YMD, ymd)
import Forceable (frc)

dayTests :: Free TestF Unit
dayTests = suite "Day" do
  test "append" do
    Assert.equal (day "AAPL" (ymd' 1) 2.0 5.0 0.5 2.0 15.0) ((day "AAPL" (ymd' 1) 2.0 3.0 1.0 3.5 10.0) <> (day "AAPL" (ymd' 2) 5.0 5.0 0.5 2.0 5.0))
    Assert.equal (day "AAPL" (ymd' 3) 5.0 5.0 0.5 3.5 15.0) ((day "AAPL" (ymd' 3) 5.0 5.0 0.5 2.0 5.0) <> (day "AAPL" (ymd' 4) 2.0 3.0 1.0 3.5 10.0))
    
ymd' :: Int -> YMD
ymd' x = frc $ ymd 1900 x x 