module Type.Purchase (Purchase, mkPurchase, buyPrice, stopPrice, priority) where

import Prelude
import Data.Maybe (Maybe(..))

newtype Purchase = Purchase 
  { buyPrice :: Number
  , stopPrice :: Number 
  , priority :: Number
  }

mkPurchase :: Number -> Number -> Number -> Maybe Purchase
mkPurchase buy stop priority = 
  if buy >= stop 
  then Just $ Purchase { buyPrice: buy, stopPrice: stop, priority: priority } 
  else Nothing

buyPrice :: Purchase -> Number
buyPrice (Purchase obj) = obj.buyPrice 

stopPrice :: Purchase -> Number
stopPrice (Purchase obj) = obj.stopPrice

priority :: Purchase -> Number
priority (Purchase obj) = obj.priority

instance showPurchase :: Show Purchase where
  show (Purchase obj) = show obj