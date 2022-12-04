module Test.URL where

import Prelude

import Control.Monad.Free (Free)
import Data.Date (Date(..), Month(..), Year(..), exactDate)
import Data.Enum (toEnum)
import Railroad (fromJust)
import Test.Unit (suite, test, TestF)
import Test.Unit.Assert as Assert
import URL (bulkURL)

urlTests :: Free TestF Unit
urlTests = suite "URL" do
  test "bulkURL" do
    Assert.equal 
      "https://eodhistoricaldata.com/api/eod-bulk-last-day/US?api_token=supersecret&fmt=json&date=2003-01-02" 
      (bulkURL "supersecret" dateJanSecond2003)
    Assert.equal 
      "https://eodhistoricaldata.com/api/eod-bulk-last-day/US?api_token=knockknock&fmt=json&date=2006-04-05" 
      (bulkURL "knockknock" dateAprilFifth2006)
  test "eodURL" do
    Assert.equal 
      "" 
      "asdf"
    Assert.equal 
      "" 
      "asdf"
  test "liveURL" do
    Assert.equal 
      "" 
      "asdf"
    Assert.equal 
      "" 
      "asdf"

dateJanSecond2003 :: Date
dateJanSecond2003 = fromJust $ exactDate (fromJust $ toEnum 2004) January (fromJust $ toEnum 2)

dateAprilFifth2006 :: Date
dateAprilFifth2006 = fromJust $ exactDate (fromJust $ toEnum 2006) April (fromJust $ toEnum 5)