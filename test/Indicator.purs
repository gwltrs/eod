module Test.Indicator where

import Prelude

import Control.Monad.Free (Free)
import Data.Array (range, take)
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
    Assert.equal ([] :: (Array String))                             (fibChunks [])
    Assert.equal (["a"])                                            (fibChunks $ take 1 alphabet)
    Assert.equal (["a", "b"])                                       (fibChunks $ take 2 alphabet)
    Assert.equal (["b", "c"])                                       (fibChunks $ take 3 alphabet)
    Assert.equal (["ab", "c", "d"])                                 (fibChunks $ take 4 alphabet)
    Assert.equal (["bc", "d", "e"])                                 (fibChunks $ take 5 alphabet)
    Assert.equal (["cd", "e", "f"])                                 (fibChunks $ take 6 alphabet)
    Assert.equal (["abc", "de", "f", "g"])                          (fibChunks $ take 7 alphabet)
    Assert.equal (["bcd", "ef", "g", "h"])                          (fibChunks $ take 8 alphabet)
    Assert.equal (["cde", "fg", "h", "i"])                          (fibChunks $ take 9 alphabet)
    Assert.equal (["def", "gh", "i", "j"])                          (fibChunks $ take 10 alphabet)
    Assert.equal (["efg", "hi", "j", "k"])                          (fibChunks $ take 11 alphabet)
    Assert.equal (["abcde", "fgh", "ij", "k", "l"])                 (fibChunks $ take 12 alphabet)
    Assert.equal (["bcdef", "ghi", "jk", "l", "m"])                 (fibChunks $ take 13 alphabet)
    Assert.equal (["cdefg", "hij", "kl", "m", "n"])                 (fibChunks $ take 14 alphabet)
    Assert.equal (["defgh", "ijk", "lm", "n", "o"])                 (fibChunks $ take 15 alphabet)
    Assert.equal (["efghi", "jkl", "mn", "o", "p"])                 (fibChunks $ take 16 alphabet)
    Assert.equal (["fghij", "klm", "no", "p", "q"])                 (fibChunks $ take 17 alphabet)
    Assert.equal (["ghijk", "lmn", "op", "q", "r"])                 (fibChunks $ take 18 alphabet)
    Assert.equal (["hijkl", "mno", "pq", "r", "s"])                 (fibChunks $ take 19 alphabet)
    Assert.equal (["abcdefgh", "ijklm", "nop", "qr", "s", "t"])     (fibChunks $ take 20 alphabet)
    Assert.equal (["bcdefghi", "jklmn", "opq", "rs", "t", "u"])     (fibChunks $ take 21 alphabet)
    Assert.equal (["cdefghij", "klmno", "pqr", "st", "u", "v"])     (fibChunks $ take 22 alphabet)
    Assert.equal (["defghijk", "lmnop", "qrs", "tu", "v", "w"])     (fibChunks $ take 23 alphabet)
    Assert.equal (["efghijkl", "mnopq", "rst", "uv", "w", "x"])     (fibChunks $ take 24 alphabet)
    Assert.equal (["fghijklm", "nopqr", "stu", "vw", "x", "y"])     (fibChunks $ take 25 alphabet)
    Assert.equal (["ghijklmn", "opqrs", "tuv", "wx", "y", "z"])     (fibChunks alphabet)

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