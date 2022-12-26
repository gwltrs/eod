module Test.Type.YMD where

import Prelude

import Control.Monad.Free (Free)
import Data.Maybe (Maybe(..))
import Effect.Class (liftEffect)
import Forceable (frc, ($!))
import Test.QuickCheck (quickCheck, (<?>))
import Test.Unit (suite, test, TestF)
import Test.Unit.Assert as Assert
import Type.YMD as Y

ymdTests :: Free TestF Unit
ymdTests = suite "YMD" do
  test "show" do
    Assert.equal "2022-12-25" (show $! Y.ymd 2022 12 25)
    Assert.equal "0001-02-03" (show $! Y.ymd 1 2 3)
  test "parse" do
    Assert.equal (Y.ymd 2022 12 25) (Y.parse "2022-12-25")
    Assert.equal (Y.ymd 1 2 3) (Y.parse "0001-02-03")
  test "JSON parsing round-trip" do
    liftEffect $ quickCheck (\d -> 
      let bool = (Just d) == (Y.parse $ show d)
      in bool <?> show d)