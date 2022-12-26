module Test.URL where

import Prelude

import Control.Monad.Free (Free)
import Data.Date (Date, Month(..), exactDate)
import Data.Enum (toEnum)
import Forceable (frc)
import Railroad (unsafeJust)
import Test.Unit (suite, test, TestF)
import Test.Unit.Assert as Assert
import Type.YMD (YMD(..), ymd)
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

dateJanSecond2003 :: YMD
dateJanSecond2003 = frc $ ymd 2003 1 2

dateDecemberFifteenth2006 :: YMD
dateDecemberFifteenth2006 = frc $ ymd 2006 12 15