module Utils where

import Prelude

import Class.RandomAccess (class RandomAccess, rAt, rLen)
import Data.CodePoint.Unicode (isAlpha)
import Data.Date (Date, Month(..), exactDate)
import Data.Enum (toEnum)
import Data.Foldable (class Foldable, foldr, intercalate)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Slice (Slice, sempty, slen, slice, sskip, stail, stake)
import Data.String (length, toCodePointArray)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (unfoldr)

allTrue :: Array Boolean -> Boolean
allTrue = foldr (&&) true

isAlphaStr :: String -> Boolean
isAlphaStr = toCodePointArray >>> (_ <#> isAlpha) >>> allTrue

toJSONArray :: forall a. (a -> String) -> Array a -> String
toJSONArray f a = 
  let inner = a <#> f # intercalate ","
  in "[" <> inner <> "]"

slices :: forall a. Int -> Slice a -> Array (Slice a)
slices n s0 = 
  let f s = if slen s >= n && n > 0 then Just (Tuple (stake n s) (fromMaybe sempty (stail s))) else Nothing
  in unfoldr f s0

-- mapSlices2 :: forall a b r. RandomAccess r => (a -> a -> b) -> r a -> Array b
-- mapSlices2 a b = []
  --let inner i a b = if i < rLen r then [] else []

uncurryRA :: forall a b r. RandomAccess r => (a -> a -> b) -> r a -> b
uncurryRA f r = f (rAt 0 r) (rAt 1 r)

infixr 0 uncurryRA as $$

doubleMap :: forall f g a b. Functor f => Functor g => (a -> b) -> f (g a) -> f (g b)
doubleMap a b = (a <$> _) <$> b

infixl 4 doubleMap as <<$>>

doubleMapFlipped :: forall f g a b. Functor f => Functor g => f (g a) -> (a -> b) -> f (g b)
doubleMapFlipped a b = doubleMap b a

infixl 1 doubleMapFlipped as <<#>>

slastN :: forall a. Int -> Slice a -> Slice a
slastN i s = sskip (slen s - i) s

slastN' :: forall a. Int -> Array a -> Slice a
slastN' i a = slastN i (slice a)

sdrop :: forall a. Int -> Slice a -> Slice a
sdrop n s = stake (slen s - n) s