module Utils where

import Prelude

import Class.RandomAccess (class RandomAccess, rAt, rLen)
import Data.Array (filter)
import Data.CodePoint.Unicode (isAlpha)
import Data.Date (Date, Month(..), exactDate)
import Data.Enum (toEnum)
import Data.Foldable (class Foldable, foldr, intercalate)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Slice (Slice, sempty, slen, slice, sskip, stail, stake)
import Data.String (length, toCodePointArray)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (unfoldr)
import Forceable (frc)
import Partial.Unsafe (unsafeCrashWith)
import Control.Apply (lift2)

--doubleApplySecond :: forall f g a b. Apply f => Apply g => f (g a) -> f (g b) -> f (g b)
--doubleApplySecond a b = a *> (_ *> b)

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

slices' :: forall a. Int -> Array a -> Array (Slice a)
slices' n s = slices n (slice s)

-- mapSlices2 :: forall a b r. RandomAccess r => (a -> a -> b) -> r a -> Array b
-- mapSlices2 a b = []
  --let inner i a b = if i < rLen r then [] else []

uncurryRA :: forall a b r. RandomAccess r => (a -> a -> b) -> r a -> b
uncurryRA f r = f (rAt 0 r) (rAt 1 r)

infixr 0 uncurryRA as $$

slastN :: forall a. Int -> Slice a -> Slice a
slastN i s = sskip (slen s - i) s

slastN' :: forall a. Int -> Array a -> Slice a
slastN' i a = slastN i (slice a)

sdrop :: forall a. Int -> Slice a -> Slice a
sdrop n s = stake (slen s - n) s

stail' :: forall a. Slice a -> Slice a
stail' s = if slen s > 0 then frc $ stail s else s

undefined :: forall a b. a -> b
undefined _ = unsafeCrashWith "undefined"

filterMap :: forall a b. (a -> Maybe b) -> Array a -> Array b
filterMap f a = a <#> f # filter isJust <#> frc

filterMaybe :: forall a. (a -> Boolean) -> Maybe a -> Maybe a
filterMaybe _ Nothing = Nothing
filterMaybe f (Just a) = if f a then Just a else Nothing