module Test.Forceable where

import Prelude

import Control.Monad.Free (Free)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Slice (slice)
import Forceable (frc)
import Test.Unit (suite, test, TestF)
import Test.Unit.Assert as Assert

forceableTests :: Free TestF Unit
forceableTests = suite "Indicator" do
  test "force Maybe" do
    Assert.equal "a" (frc $ Just "a")
  test "force Either" do
    Assert.equal "b" (frc $ Right "b")
  test "force Array" do
    Assert.equal "c" (frc $ ["c"])
  test "force Slice" do
    Assert.equal "d" (frc $ slice ["d"])