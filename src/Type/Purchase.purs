module Type.Purchase (Purchase(..)) where

import Prelude

newtype Purchase = Purchase { buyPrice :: Number, stopPrice :: Number }