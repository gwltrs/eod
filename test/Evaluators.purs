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
import Type.Purchase (Purchase, mkPurchase)
import Forceable (($!), frc)
import Type.Alias (RMultiple)
import NestedApplicative ((<<*>>))
import Partial.Unsafe (unsafeCrashWith)

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
    withinHundredth 0.0 (maxPreviousLow' (mkP 2.0 1.0) [])
    withinHundredth 1.0 (maxPreviousLow' (mkP 2.0 1.0) [fourPrice 3.0])
    withinHundredth 0.0 (maxPreviousLow' (mkP 2.0 1.0) [fourPrice 2.0])
    withinHundredth (-1.0) (maxPreviousLow' (mkP 2.0 1.0) [fourPrice 1.0])
    withinHundredth (-2.0) (maxPreviousLow' (mkP 2.0 1.0) [fourPrice 0.0])

maxPreviousLow' :: Purchase -> Array Day -> RMultiple
maxPreviousLow' p a = 
  case (evaluate' maxPreviousLow a) <*> (Just p) of
    (Just r) -> r
    Nothing -> unsafeCrashWith "maxPreviousLow' returned Nothing"

mkP :: Number -> Number -> Purchase
mkP buy stop = frc $ mkPurchase buy stop

withinHundredth :: Number -> Number -> Test
withinHundredth expected actual =
  Assert.assert 
    ("Error: expected " <> show expected <> ", got " <> show actual)
    (eqAbsolute (Tolerance 0.01) expected actual)

doji :: Day
doji = day 10.0 12.0 8.0 10.0 1000.0