module Test.Indicator where

import Prelude

import Control.Monad.Free (Free)
import Data.Array (range)
import Data.Foldable (sum)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Slice (slice)
import Forceable (frc)
import Indicators (lastPrice, sma)
import Test.Unit (suite, test, TestF)
import Test.Unit.Assert as Assert
import Type.Indicator (Indicator, indicate, indicate', indicator, shiftLeft)
import Type.LiveDay (LiveDay, noMove)

indicatorTests :: Free TestF Unit
indicatorTests = suite "Indicator" do
  test "indicate, sma" do
    Assert.equal (Just 9.0) (indicate' (sma 3) noMoves1to10)
    Assert.equal (Just 5.5) (indicate' (sma 10) noMoves1to10)
    Assert.equal Nothing (indicate' (sma 11) noMoves1to10)
  test "lastPrice, shiftLeft" do
    Assert.equal (Just 10.0) (indicate' lastPrice noMoves1to10)
    Assert.equal (Just 10.0) (indicate' (shiftLeft 0 lastPrice) noMoves1to10)
    Assert.equal (Just 9.0) (indicate' (shiftLeft 1 lastPrice) noMoves1to10)
    Assert.equal (Just 8.0) (indicate' (shiftLeft 2 lastPrice) noMoves1to10)
    Assert.equal (Just 2.0) (indicate' (shiftLeft 8 lastPrice) noMoves1to10)
    Assert.equal (Just 1.0) (indicate' (shiftLeft 9 lastPrice) noMoves1to10)
    Assert.equal Nothing (indicate' (shiftLeft 10 lastPrice) noMoves1to10)
  test "Functor instance" do
    Assert.equal 
      (Just 16.0) 
      (indicate' (sma 3 # shiftLeft 1 <#> (_ * 2.0)) noMoves1to10)
    Assert.equal 
      Nothing 
      (indicate' (sma 3 # shiftLeft 9 <#> (_ * 2.0)) noMoves1to10)
  test "Applicative instance" do
    Assert.equal 
      (Just 54.0) 
      (indicate' ((*) <$> sma 3 <*> (sma 3 # shiftLeft 3)) noMoves1to10)
    Assert.equal 
      Nothing 
      (indicate' ((*) <$> sma 3 <*> (sma 3 # shiftLeft 10)) noMoves1to10)

noMoves1to10 :: Array LiveDay
noMoves1to10 = noMove <$> toNumber <$> range 1 10