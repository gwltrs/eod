module Test.Indicator where

import Prelude

import Control.Monad.Free (Free)
import Data.Array (range)
import Data.Foldable (sum)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Slice (slice)
import Forceable (frc)
import Indicators (convex, at, fibChunks)
import Test.Unit (suite, test, TestF)
import Test.Unit.Assert as Assert
import Type.Indicator (Indicator, indicate, indicate')
import Type.Day (Day, day, avg, fourPrice, close)

indicatorTests :: Free TestF Unit
indicatorTests = suite "Indicator" do
  test "at" do
    Assert.equal Nothing                  (indicate' (at 0) $ [])
    Assert.equal (Just doji)              (indicate' (at 0) $ [doji])
    Assert.equal Nothing                  (indicate' (at 1) $ [doji])
    Assert.equal (Just $ fourPrice 3.0)   (indicate' (at 0) $ [doji, fourPrice 3.0])
    Assert.equal (Just doji)              (indicate' (at 1) $ [doji, fourPrice 3.0])
    Assert.equal Nothing                  (indicate' (at 2) $ [doji, fourPrice 3.0])
  test "convex" do
    Assert.equal 0 (convex (toNumber <$> range 1 20))
    Assert.equal 0 (convex (toNumber <$> range 20 1))
    Assert.equal 0 (convex [1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 3.0, 5.0, 4.0])
    Assert.equal 3 (convex [1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 2.0, 4.0, 8.0])
    Assert.equal 4 (convex [1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 4.0, 1.0, 0.0, 1.0])
    Assert.equal 5 (convex [1.0, 1.0, 1.0, 1.0, 1.0, 16.0, 4.0, 1.0, 0.0, 1.0])
    Assert.equal 0 (convex [1.0, 1.0, 1.0, 1.0, 1.0, 16.0, 4.0, 1.0, 10.0, 1.0])
    Assert.equal 10 (convex [4.0, 2.0, 1.0, 2.0, 4.0, 8.0, 16.0, 32.0, 64.0, 128.0])
    Assert.equal 11 (convex [8.0, 4.0, 2.0, 1.0, 2.0, 4.0, 8.0, 16.0, 32.0, 64.0, 128.0])
  test "fibChunks" do
    Assert.equal ([] :: (Array String))                         (fibChunks (-1) alphabet)
    Assert.equal ([] :: (Array String))                         (fibChunks 0 alphabet)
    Assert.equal ["z"]                                          (fibChunks 1 alphabet)
    Assert.equal ["y", "z"]                                     (fibChunks 2 alphabet)
    Assert.equal ["wx", "y", "z"]                               (fibChunks 3 alphabet)
    Assert.equal ["tuv", "wx", "y", "z"]                        (fibChunks 4 alphabet)
    Assert.equal ["opqrs", "tuv", "wx", "y", "z"]               (fibChunks 5 alphabet)
    Assert.equal ["ghijklmn", "opqrs", "tuv", "wx", "y", "z"]   (fibChunks 6 alphabet)
    Assert.equal ["ghijklmn", "opqrs", "tuv", "wx", "y", "z"]   (fibChunks 7 alphabet)
    Assert.equal ["ghijklmn", "opqrs", "tuv", "wx", "y", "z"]   (fibChunks 8 alphabet)

doji :: Day
doji = day 10.0 12.0 8.0 10.0 1000.0

fourPrices1to10 :: Array Day
fourPrices1to10 = fourPrice <$> toNumber <$> range 1 10

fourPrices1to20 :: Array Day
fourPrices1to20 = fourPrice <$> toNumber <$> range 1 20

fourPrices20to1 :: Array Day
fourPrices20to1 = fourPrice <$> toNumber <$> range 20 1

alphabet :: Array String
alphabet = ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"]