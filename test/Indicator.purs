module Test.Indicator where

import Prelude

import Control.Monad.Free (Free)
import Data.Array (range)
import Data.Foldable (sum)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Slice (slice)
import Forceable (frc)
import Indicators (convex, day, sma)
import Test.Unit (suite, test, TestF)
import Test.Unit.Assert as Assert
import Type.Indicator (Indicator, indicate, indicate', indicator, shiftLeft, (<<))
import Type.LiveDay (LiveDay, noMove)

indicatorTests :: Free TestF Unit
indicatorTests = suite "Indicator" do
  test "indicate, sma" do
    Assert.equal (Just 9.0) (indicate' (sma 3) noMoves1to10)
    Assert.equal (Just 5.5) (indicate' (sma 10) noMoves1to10)
    Assert.equal Nothing (indicate' (sma 11) noMoves1to10)
  test "day, shiftLeft" do
    Assert.equal (Just 10.0) (indicate' (day <#> _.close) noMoves1to10)
    Assert.equal (Just 10.0) (indicate' (shiftLeft 0 (day <#> _.close)) noMoves1to10)
    Assert.equal (Just 9.0) (indicate' (shiftLeft 1 (day <#> _.close)) noMoves1to10)
    Assert.equal (Just 8.0) (indicate' (shiftLeft 2 (day <#> _.close)) noMoves1to10)
    Assert.equal (Just 2.0) (indicate' (shiftLeft 8 (day <#> _.close)) noMoves1to10)
    Assert.equal (Just 1.0) (indicate' (shiftLeft 9 (day <#> _.close)) noMoves1to10)
    Assert.equal Nothing (indicate' (shiftLeft 10 (day <#> _.close)) noMoves1to10)
  test "Functor instance" do
    Assert.equal 
      (Just 16.0) 
      (indicate' (sma 3 << 1 <#> (_ * 2.0)) noMoves1to10)
    Assert.equal 
      Nothing 
      (indicate' (sma 3 # shiftLeft 9 <#> (_ * 2.0)) noMoves1to10)
  test "Applicative instance" do
    Assert.equal 
      (Just 54.0) 
      (indicate' ((*) <$> sma 3 <*> (sma 3 << 3)) noMoves1to10)
    Assert.equal 
      Nothing 
      (indicate' ((*) <$> sma 3 <*> (sma 3 # shiftLeft 10)) noMoves1to10)
  test "convex" do
    Assert.equal Nothing (frc $ indicate' convex noMoves1to20)
    Assert.equal Nothing (frc $ indicate' convex noMoves20to1)
    Assert.equal Nothing (frc $ indicate' convex $ noMove <$> [1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 3.0, 5.0, 4.0])
    Assert.equal (Just 3) (frc $ indicate' convex $ noMove <$> [1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 2.0, 4.0, 8.0])
    Assert.equal (Just 4) (frc $ indicate' convex $ noMove <$> [1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 4.0, 1.0, 0.0, 1.0])
    Assert.equal (Just 5) (frc $ indicate' convex $ noMove <$> [1.0, 1.0, 1.0, 1.0, 1.0, 16.0, 4.0, 1.0, 0.0, 1.0])
    Assert.equal Nothing (frc $ indicate' convex $ noMove <$> [1.0, 1.0, 1.0, 1.0, 1.0, 16.0, 4.0, 1.0, 10.0, 1.0])
    Assert.equal (Just 10) (frc $ indicate' convex $ noMove <$> [4.0, 2.0, 1.0, 2.0, 4.0, 8.0, 16.0, 32.0, 64.0, 128.0])
    Assert.equal (Just 10) (frc $ indicate' convex $ noMove <$> [8.0, 4.0, 2.0, 1.0, 2.0, 4.0, 8.0, 16.0, 32.0, 64.0, 128.0])

noMoves1to10 :: Array LiveDay
noMoves1to10 = noMove <$> toNumber <$> range 1 10

noMoves1to20 :: Array LiveDay
noMoves1to20 = noMove <$> toNumber <$> range 1 20

noMoves20to1 :: Array LiveDay
noMoves20to1 = noMove <$> toNumber <$> range 20 1