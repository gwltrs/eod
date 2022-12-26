module Type.YMD where

import Prelude

import Data.Date (Date, Day, Month, Year, day, exactDate, month, year)
import Data.Enum (fromEnum, toEnum)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (unwrap, wrap)
import Data.String (length)
import Data.String.CodeUnits (slice)
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

parse :: String -> Maybe YMD
parse str = do
  y <- fromString $ slice 0 4 str
  m <- fromString $ slice 5 7 str
  d <- fromString $ slice 8 10 str
  ymd y m d

instance eqYMD :: Eq YMD where
  eq (YMD a) (YMD b) = eq a b

instance arbYMD :: Arbitrary YMD where
  arbitrary = 
    let arbYear = chooseInt 0 3000 <#> (frc <<< toEnum)
    in YMD <$> frc <$> suchThat (exactDate <$> arbYear <*> enum <*> enum) isJust

instance showYMD :: Show YMD where
  show (YMD date) = 
    let 
      padZeros n s = if length s < n then padZeros n ("0" <> s) else s
      y = year date # fromEnum # show # padZeros 4
      m = month date # fromEnum # show # padZeros 2
      d = day date # fromEnum # show # padZeros 2
    in 
      y <> "-" <> m <> "-" <> d