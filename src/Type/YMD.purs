module Type.YMD where

import Prelude

import Data.Date (Date, Day, Month, Year, day, exactDate, month, year)
import Data.Enum (toEnum)
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (unwrap, wrap)
import Forceable (frc)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (chooseInt, enum, suchThat)

newtype YMD = YMD Date

ymd :: Int -> Int -> Int -> Maybe YMD
ymd y m d = do
  year <- toEnum y
  month <- toEnum m 
  day <- toEnum d
  YMD <$> exactDate year month day

fromJSON :: String -> Maybe YMD
fromJSON json = Nothing

toJSON :: YMD -> String
toJSON date = ""

instance eqYMD :: Eq YMD where
  eq (YMD a) (YMD b) = a == b

instance arbYMD :: Arbitrary YMD where
  arbitrary = 
    let arbYear = chooseInt 0 3000 <#> (frc <<< toEnum)
    in YMD <$> frc <$> suchThat (exactDate <$> arbYear <*> enum <*> enum) isJust

instance showYMD :: Show YMD where
  show (YMD d) = show d