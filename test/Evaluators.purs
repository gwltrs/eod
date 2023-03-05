module Test.Evaluators where

import Prelude
import Control.Monad.Free (Free)
import Test.Unit (suite, test, TestF)
import Test.Unit.Assert as Assert
import Type.YMD (ymd)
import Forceable (frc)
import Evaluators (expectancy)

evaluatorsTests :: Free TestF Unit
evaluatorsTests = suite "Evaluators" do
  test "expectancy" do
   Assert.equal 0.49 (expectancy table2_6)

table2_6 :: Array Number
table2_6 = [
  6.58, 7.7, 1.38, 2.23, 7.53, 2.2, -(1.09), -(1.4), -(0.85), 1.71, -(2.58), 
  -(0.11), -(0.1), -(1.02), -(0.81), 0.26, -(1.4), -(1.0), -(1.2), -(3.56), -(2.8), -(1.0)]