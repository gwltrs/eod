module Test.Evaluators where

import Prelude
import Control.Monad.Free (Free)
import Test.Unit (suite, test, TestF)
import Test.Unit.Assert as Assert
import Test.Unit (Test)
import Data.Int (toNumber)
import Data.Number.Approximate (eqAbsolute, Tolerance(..))
import Type.Day (Day, day, fourPrice)
import Data.Maybe (Maybe(..))
import Type.Evaluator (evaluate, evaluate')
import Evaluators (at, maxPreviousLow)

evaluatorsTests :: Free TestF Unit
evaluatorsTests = suite "Evaluators" do
  test "at" do
    Assert.equal Nothing                  (evaluate' (at 0) $ [])
    Assert.equal (Just doji)              (evaluate' (at 0) $ [doji])
    Assert.equal Nothing                  (evaluate' (at 1) $ [doji])
    Assert.equal (Just doji)              (evaluate' (at 0) $ [doji, fourPrice 3.0])
    Assert.equal (Just $ fourPrice 3.0)   (evaluate' (at 1) $ [doji, fourPrice 3.0])
    Assert.equal Nothing                  (evaluate' (at 2) $ [doji, fourPrice 3.0])
  test "maxPreviousLow" do
    Assert.equal 1 2

withinHundredth :: Number -> Number -> Test
withinHundredth expected actual =
  Assert.assert 
    ("Error: expected " <> show expected <> ", got " <> show actual)
    (eqAbsolute (Tolerance 0.01) expected actual)

doji :: Day
doji = day 10.0 12.0 8.0 10.0 1000.0