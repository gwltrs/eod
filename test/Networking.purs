module Test.Networking where

import Prelude

import Control.Monad.Free (Free)
import Data.Maybe (Maybe(..))
import Networking (parseDay)
import Test.Unit (suite, test, TestF)
import Test.Unit.Assert as Assert

networkingTests :: Free TestF Unit
networkingTests = suite "Networking" do
  test "parseDay" do
    Assert.equal 
      Nothing
      (parseDay "")
    Assert.equal 
      (Just { date: "2022-12-01", open: 148.25, high: 149.125, low: 146.5, close: 148.0, volume: 68230295.0 })
      (parseDay """{"code":"AAPL","exchange_short_name":"US","date":"2022-12-01","open":148.25,"high":149.125,"low":146.50,"close":148,"adjusted_close":148.77,"volume":68230295}""")