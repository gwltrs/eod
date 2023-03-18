module Test.Evaluators where

import Prelude
import Control.Monad.Free (Free)
import Test.Unit (suite, test, TestF)
import Test.Unit.Assert as Assert
import Test.Unit (Test)
import Data.Int (toNumber)
import Data.Number.Approximate (eqAbsolute, Tolerance(..))

import Evaluators (at, maxPreviousLow)

evaluatorsTests :: Free TestF Unit
evaluatorsTests = suite "Evaluators" do
  test "at" do
    Assert.equal 1 2

withinHundredth :: Number -> Number -> Test
withinHundredth expected actual =
  Assert.assert 
    ("Error: expected " <> show expected <> ", got " <> show actual)
    (eqAbsolute (Tolerance 0.01) expected actual)