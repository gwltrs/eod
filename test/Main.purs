module Test.Main where

import Prelude

import Effect (Effect)
import Test.Type.BulkDay (bulkDayTests)
import Test.Type.EODDay (eodDayTests)
import Test.Type.LiveDay (liveDayTests)
import Test.Type.YMD (ymdTests)
import Test.URL (urlTests)
import Test.Unit.Main (runTest)
import Test.Utils (utilsTests)

main :: Effect Unit
main = runTest do
  bulkDayTests
  eodDayTests
  liveDayTests
  urlTests
  ymdTests
  utilsTests