module Test.Main where

import Prelude

import Effect (Effect)
import Test.Forceable (forceableTests)
import Test.Indicator (indicatorTests)
import Test.Type.BulkDay (bulkDayTests)
import Test.Type.EODDay (eodDayTests)
import Test.Type.Day (dayTests)
import Test.Type.YMD (ymdTests)
import Test.URL (urlTests)
import Test.Unit.Main (runTest)
import Test.Utils (utilsTests)
import Test.SystemQuality (systemQualityTests)
import Test.Evaluators (evaluatorsTests)

main :: Effect Unit
main = runTest do
  bulkDayTests
  eodDayTests
  dayTests
  urlTests
  ymdTests
  utilsTests
  indicatorTests
  forceableTests
  systemQualityTests
  evaluatorsTests