module Test.Utils where

import Prelude

import Control.Monad.Free (Free)
import Data.Maybe (Maybe(..))
import Data.Slice (Slice, sarray, sempty, slice, stail, stake)
import Effect.Class (liftEffect)
import Forceable (frc, ($!))
import Test.QuickCheck (quickCheck, (<?>))
import Test.Unit (suite, test, TestF)
import Test.Unit.Assert as Assert
import Type.YMD as Y
import Utils (sdrop, slastN, slices)

utilsTests :: Free TestF Unit
utilsTests = suite "Utils" do
  test "slices" do
    -- vowels
    Assert.equal [] (slices 0 vowels <#> sarray)
    Assert.equal [["a"], ["e"], ["i"], ["o"], ["u"]] (slices 1 vowels <#> sarray)
    Assert.equal [["a", "e"], ["e", "i"], ["i", "o"], ["o", "u"]] (slices 2 vowels <#> sarray)
    Assert.equal [["a", "e", "i"], ["e", "i", "o"], ["i", "o", "u"]] (slices 3 vowels <#> sarray)
    Assert.equal [["a", "e", "i", "o"], ["e", "i", "o", "u"]] (slices 4 vowels <#> sarray)
    Assert.equal [["a", "e", "i", "o", "u"]] (slices 5 vowels <#> sarray)
    Assert.equal [] (slices 6 vowels <#> sarray)
    -- bools
    Assert.equal [] (slices 0 bools <#> sarray)
    Assert.equal [[false], [true]] (slices 1 bools <#> sarray)
    Assert.equal [[false, true]] (slices 2 bools <#> sarray)
    Assert.equal [] (slices 3 bools <#> sarray)
    -- units
    Assert.equal [] (slices 0 units <#> sarray)
    Assert.equal [[unit]] (slices 1 units <#> sarray)
    Assert.equal [] (slices 2 units <#> sarray)
    -- empty
    Assert.equal [] (slices 0 noUnits <#> sarray)
    Assert.equal [] (slices 1 noUnits <#> sarray)
  -- test "mapSlices2" do
  --   -- vowels
  --   Assert.equal ([]::Array Int) (mapSlices2 (+) [])
  --   Assert.equal [] (mapSlices2 (+) [0])
  --   Assert.equal [1] (mapSlices2 (+) [0, 1])
  --   Assert.equal [1, 3] (mapSlices2 (+) [0, 1, 2])
  --   Assert.equal [1, 3, 5] (mapSlices2 (+) [0, 1, 2, 3])
  --   Assert.equal [1, 3, 5, 7] (mapSlices2 (+) [0, 1, 2, 3, 4])
  test "slastN" do
    -- vowels
    Assert.equal [] (slastN 0 vowels # sarray)
    Assert.equal ["u"] (slastN 1 vowels # sarray)
    Assert.equal ["o", "u"] (slastN 2 vowels # sarray)
    Assert.equal ["i", "o", "u"] (slastN 3 vowels # sarray)
    Assert.equal ["e", "i", "o", "u"] (slastN 4 vowels # sarray)
    Assert.equal ["a", "e", "i", "o", "u"] (slastN 5 vowels # sarray)
    Assert.equal ["a", "e", "i", "o", "u"] (slastN 6 vowels # sarray)
    -- bools
    Assert.equal [] (slastN 0 bools # sarray)
    Assert.equal [true] (slastN 1 bools # sarray)
    Assert.equal [false, true] (slastN 2 bools # sarray)
    Assert.equal [false, true] (slastN 3 bools # sarray)
    -- units
    Assert.equal [] (slastN 0 units # sarray)
    Assert.equal [unit] (slastN 1 units # sarray)
    Assert.equal [unit] (slastN 2 units # sarray)
    -- empty
    Assert.equal [] (slastN 0 noUnits # sarray)
    Assert.equal [] (slastN 1 noUnits # sarray)
  test "sdrop" do
        -- vowels
    Assert.equal ["a", "e", "i", "o", "u"] (sdrop 0 vowels # sarray)
    Assert.equal ["a", "e", "i", "o"] (sdrop 1 vowels # sarray)
    Assert.equal ["a", "e", "i"] (sdrop 2 vowels # sarray)
    Assert.equal ["a", "e"] (sdrop 3 vowels # sarray)
    Assert.equal ["a"] (sdrop 4 vowels # sarray)
    Assert.equal [] (sdrop 5 vowels # sarray)
    Assert.equal [] (sdrop 6 vowels # sarray)
    -- bools
    Assert.equal [false, true] (sdrop 0 bools # sarray)
    Assert.equal [false] (sdrop 1 bools # sarray)
    Assert.equal [] (sdrop 2 bools # sarray)
    Assert.equal [] (sdrop 3 bools # sarray)
    -- units
    Assert.equal [unit] (sdrop 0 units # sarray)
    Assert.equal [] (sdrop 1 units # sarray)
    Assert.equal [] (sdrop 2 units # sarray)
    -- empty
    Assert.equal [] (sdrop 0 noUnits # sarray)
    Assert.equal [] (sdrop 1 noUnits # sarray)

vowels :: Slice String
vowels = stake 5 $ slice ["a", "e", "i", "o", "u", "x", "y", "z"]

bools :: Slice Boolean
bools = stake 2 $ frc $ stail $ slice [false, false, true, true]

units :: Slice Unit
units = slice [unit]

noUnits :: Slice Unit
noUnits = sempty