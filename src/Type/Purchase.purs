module Type.Purchase (Purchase, mkPurchase, buyPrice, stopPrice) where

import Prelude
import Data.Maybe (Maybe(..))

mkPurchase :: Number -> Number -> Maybe Purchase
mkPurchase buy stop = if buy >= stop then Just $ Purchase { buyPrice: buy, stopPrice: stop } else Nothing

buyPrice :: Purchase -> Number
buyPrice (Purchase obj) = obj.buyPrice 

stopPrice :: Purchase -> Number
stopPrice (Purchase obj) = obj.stopPrice

newtype Purchase = Purchase { buyPrice :: Number, stopPrice :: Number }