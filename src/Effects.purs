module Effects where

import Prelude
import Effect (Effect)
import Type.YMD (YMD(..))
import Effect.Now (nowDate)

getToday :: Effect YMD
getToday = YMD <$> nowDate