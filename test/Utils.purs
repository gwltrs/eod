module Test.Utils where

import Prelude

import Control.Monad.Free (Free)
import Data.Maybe (Maybe(..))
import Data.Slice (sarray)
import Effect.Class (liftEffect)
import Forceable (frc, ($!))
import Test.QuickCheck (quickCheck, (<?>))
import Test.Unit (suite, test, TestF)
import Test.Unit.Assert as Assert
import Type.YMD as Y
import Utils (slices)

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

vowels :: Array String
vowels = ["a", "e", "i", "o", "u"]

bools :: Array Boolean
bools = [false, true]

units :: Array Unit
units = [unit]

noUnits :: Array Unit
noUnits = []