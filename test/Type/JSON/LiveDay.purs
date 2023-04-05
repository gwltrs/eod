module Test.Type.JSON.LiveDay where

import Prelude

import Control.Monad.Free (Free)
import Data.Maybe (Maybe(..))
import Test.Unit (suite, test, TestF)
import Test.Unit.Assert as Assert
import Test.Value (aaplBulkDay, aaplBulkDayJSON)

--dayTests :: Free TestF Unit
--dayTests = suite "Day" do
--  test "dayFromJSON" do
--    Assert.equal Nothing (dayFromJSON "")
--    Assert.equal (Just $ toDay $ toEODDay aaplBulkDay) (dayFromJSON aaplBulkDayJSON)