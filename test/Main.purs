module Test.Main where

import Prelude

import Effect (Effect)
import Test.Networking (networkingTests)
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest do
  networkingTests