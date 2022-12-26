module Test.Type.Date where

import Prelude

import Control.Monad.Free (Free)
import Data.Maybe (Maybe(..))
import Test.QuickCheck (quickCheck)
import Test.Unit (suite, test, TestF)
import Test.Unit.Assert as Assert
import Type.Date as D

-- dateTests :: Free TestF Unit
-- dateTests = suite "Date" do
--   test "round-trip" do
--     Assert.equal "" "asdf"
--     quickCheck (\d -> (Just d) == D.fromJSON $ D.toJSON d)