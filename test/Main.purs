module Test.Main where

import Prelude

import Effect (Effect)
import Test.Type.BulkDay (bulkDayTests)
import Test.Type.EODDay (eodDayTests)
import Test.Type.LiveDay (liveDayTests)
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest do
  bulkDayTests
  eodDayTests
  liveDayTests