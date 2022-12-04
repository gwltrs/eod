module Test.URL where

import Prelude

import Control.Monad.Free (Free)
import Data.Date (Date, Month(..), exactDate)
import Data.Enum (toEnum)
import Railroad (fromJust)
import Test.Unit (suite, test, TestF)
import Test.Unit.Assert as Assert
import URL (bulkURL, eodURL, liveURL)

urlTests :: Free TestF Unit
urlTests = suite "URL" do
  test "bulkURL" do
    Assert.equal 
      "https://eodhistoricaldata.com/api/eod-bulk-last-day/US?api_token=supersecret&fmt=json&date=2003-01-02" 
      (bulkURL "supersecret" dateJanSecond2003)
    Assert.equal 
      "https://eodhistoricaldata.com/api/eod-bulk-last-day/US?api_token=knockknock&fmt=json&date=2006-12-15" 
      (bulkURL "knockknock" dateDecemberFifteenth2006)
  test "eodURL" do
    Assert.equal 
      "https://eodhistoricaldata.com/api/eod/AAPL.US?api_token=supersecret&fmt=json&from=2003-01-02" 
      (eodURL "supersecret" dateJanSecond2003 "AAPL")
    Assert.equal 
      "https://eodhistoricaldata.com/api/eod/AMZN.US?api_token=knockknock&fmt=json&from=2006-12-15" 
      (eodURL "knockknock" dateDecemberFifteenth2006 "AMZN")
  test "liveURL" do
    Assert.equal 
      "https://eodhistoricaldata.com/api/real-time/AAPL.US?api_token=supersecret&fmt=json"
      (liveURL "supersecret" "AAPL")
    Assert.equal 
      "https://eodhistoricaldata.com/api/real-time/AMZN.US?api_token=knockknock&fmt=json" 
      (liveURL "knockknock" "AMZN")

dateJanSecond2003 :: Date
dateJanSecond2003 = fromJust $ exactDate (fromJust $ toEnum 2003) January (fromJust $ toEnum 2)

dateDecemberFifteenth2006 :: Date
dateDecemberFifteenth2006 = fromJust $ exactDate (fromJust $ toEnum 2006) December (fromJust $ toEnum 15)