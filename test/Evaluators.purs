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
    
    withinHundredth 0.0 (maxPreviousLow' 0 (mkP 2 1) [])

    withinHundredth (-1.5) (maxPreviousLow' 1 (mkP 12 10) [day' 9 11 7 8])
    withinHundredth (-1.0) (maxPreviousLow' 1 (mkP 12 10) [day' 11 12 10 11])
    withinHundredth (-1.0) (maxPreviousLow' 1 (mkP 12 10) [day' 11 12 9 11])
    withinHundredth 0.5 (maxPreviousLow' 1 (mkP 12 10) [day' 11 13 11 13])

    withinHundredth (-1.5) (maxPreviousLow' 2 (mkP 12 10) [day' 9 11 7 8, day1234])
    withinHundredth (-1.0) (maxPreviousLow' 2 (mkP 12 10) [day' 11 12 10 11, day1234])
    withinHundredth (-1.0) (maxPreviousLow' 2 (mkP 12 10) [day' 11 12 9 11, day1234])

    withinHundredth 1.5 (maxPreviousLow' 2 (mkP 12 10) [day' 11 13 11 13, day' 12 13 14 15])
    withinHundredth (-1.5) (maxPreviousLow' 2 (mkP 12 10) [day' 11 13 11 13, day' 9 15 9 15])
    withinHundredth (-0.5) (maxPreviousLow' 3 (mkP 12 10) [day' 11 13 11 13, day' 12 15 5 15, day1234])
    withinHundredth 0.0 (maxPreviousLow' 3 (mkP 12 10) [day' 11 13 11 13, day' 12 14 12 14, day' 13 14 10 13]) 
    withinHundredth 2.0 (maxPreviousLow' 3 (mkP 12 10) [day' 11 13 11 13, day' 12 14 12 14, day' 13 14 13 16]) 
    
maxPreviousLow' :: Int -> Purchase -> Array Day -> RMultiple
maxPreviousLow' n p a = 
  case (evaluate' (maxPreviousLow n) a) <*> (Just p) of
    (Just r) -> r
    Nothing -> unsafeCrashWith "maxPreviousLow' returned Nothing"

mkP :: Int -> Int -> Purchase
mkP buy stop = frc $ mkPurchase (toNumber buy) (toNumber stop) 0.0

day' :: Int -> Int -> Int -> Int -> Day
day' o h l c = day (toNumber o) (toNumber h) (toNumber l) (toNumber c) 1.0

withinHundredth :: Number -> Number -> Test
withinHundredth expected actual =
  Assert.assert 
    ("Error: expected " <> show expected <> ", got " <> show actual)
    (eqAbsolute (Tolerance 0.01) expected actual)

day1234 :: Day
day1234 = day' 1 2 3 4

doji :: Day
doji = day 10.0 12.0 8.0 10.0 1000.0