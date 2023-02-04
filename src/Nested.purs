module Nested where

import Prelude
import Control.Apply (lift2, applyFirst, applySecond)

nestedMap :: forall f g a b. Functor f => Functor g => (a -> b) -> f (g a) -> f (g b)
nestedMap = compose map map
--nestedMap f a = (f <$> _) <$> a

infixl 4 nestedMap as <<$>>

nestedMapFlipped :: forall f g a b. Functor f => Functor g => f (g a) -> (a -> b) -> f (g b)
nestedMapFlipped = flip nestedMap

infixl 1 nestedMapFlipped as <<#>>

nestedApply :: forall f g a b. Apply f => Apply g => f (g (a -> b)) -> f (g a) -> f (g b)
nestedApply = lift2 apply

infixl 4 nestedApply as <<*>>

nestedApplyFirst :: forall f g a b. Apply f => Apply g => f (g a) -> f (g b) -> f (g a)
nestedApplyFirst = lift2 applyFirst

infixl 4 nestedApplyFirst as <<*

nestedApplySecond :: forall f g a b. Apply f => Apply g => f (g a) -> f (g b) -> f (g b)
nestedApplySecond = lift2 applySecond

infixl 4 nestedApplySecond as *>>

nestedPure :: forall f g a. Applicative f => Applicative g => a -> f (g a)
nestedPure = compose pure pure