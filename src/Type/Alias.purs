module Type.Alias where

import Control.Monad.Except (ExceptT)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Exception (Error)

type AffE a = ExceptT Error Aff a
type APIKey = String
type Ticker = String
type URL = String
type RMultiple = Number