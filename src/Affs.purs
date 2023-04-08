module Affs where

import Prelude
import Effect.Aff (Aff, delay, Milliseconds(..))

delaySeconds :: Number -> Aff Unit
delaySeconds seconds = delay $ Milliseconds $ seconds * 1000.0