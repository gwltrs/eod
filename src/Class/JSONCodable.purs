module Class.JSONCodable where

import Prelude

import Data.Maybe

class JSONCodable a where
  toJSON :: a -> String
  fromJSON :: String -> Maybe a