module Class.Day where

import Prelude

class Day d where
  open :: d -> Number
  high :: d -> Number
  low :: d -> Number
  close :: d -> Number
  volume :: d -> Number