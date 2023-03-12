module Test.Evaluators where

import Prelude
import Control.Monad.Free (Free)
import Test.Unit (suite, test, TestF)
import Test.Unit.Assert as Assert
import Test.Unit (Test)
import Type.YMD (ymd)
import Forceable (frc)
import Evaluators (expectancy, systemQuality, standardDeviation)
import Data.Int (toNumber)
import Data.Number.Approximate (eqAbsolute, Tolerance(..))

evaluatorsTests :: Free TestF Unit
evaluatorsTests = suite "Evaluators" do
  test "expectancy" do
    withinHundredth 0.72 (expectancy table3_6)
    withinHundredth 0.0 (expectancy [])
    withinHundredth 1.0 (expectancy [1.0])
    withinHundredth 0.0 (expectancy [0.0])
    withinHundredth (-(1.0)) (expectancy [-(1.0)])
  test "systemQuality" do
    withinHundredth 0.75 (systemQuality table3_6)
    withinHundredth 0.0 (systemQuality [])
  test "standardDeviation" do
    withinHundredth 4.81 (standardDeviation table3_6)
    withinHundredth 0.0 (standardDeviation [])
    withinHundredth 0.0 (standardDeviation [1.0])
    withinHundredth 0.0 (standardDeviation [0.0])
    withinHundredth 0.5 (standardDeviation [1.0, 2.0])

table3_6 :: Array Number
table3_6 = [-1,-1,-1,-1,-1,-1,10,-1,-5,-5,-1,-1,-1,-1,10,-1,-5,-1,10,-1,10,-1,-1,10,-1]
  <#> toNumber

withinHundredth :: Number -> Number -> Test
withinHundredth expected actual =
  Assert.assert 
    ("Error: expected " <> show expected <> ", got " <> show actual)
    (eqAbsolute (Tolerance 0.01) expected actual)