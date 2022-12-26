module Test.Type.YMD where

import Prelude

import Control.Monad.Free (Free)
import Data.Maybe (Maybe(..))
import Effect.Class (liftEffect)
import Test.QuickCheck (quickCheck, (<?>))
import Test.Unit (suite, test, TestF)
import Test.Unit.Assert as Assert
import Type.YMD as Y

ymdTests :: Free TestF Unit
ymdTests = suite "YMD" do
  test "JSON parsing round-trip" do
    Assert.equal "" ""
    liftEffect $ quickCheck (\d -> 
      let bool = (Just d) == (Y.fromJSON $ Y.toJSON d)
      in bool <?> show d)