module Type.Date where

import Prelude

import Data.Date (Date, exactDate)
import Data.Enum (toEnum)
import Data.Maybe (Maybe(..), isJust)
import Railroad (unsafeJust)
import Test.QuickCheck (class Arbitrary)
import Test.QuickCheck.Gen (Gen, enum, suchThat)

date :: Int -> Int -> Int -> Maybe Date
date y m d = do
  year <- toEnum y
  month <- toEnum m
  day <- toEnum d
  exactDate year month day

unsafeDate :: Int -> Int -> Int -> Date
unsafeDate y m d = unsafeJust $ date y m d

fromJSON :: String -> Maybe Date
fromJSON json = Nothing

toJSON :: Date -> String
toJSON date = ""

arb :: Gen Date
arb = unsafeJust <$> suchThat (exactDate <$> enum <*> enum <*> enum) isJust