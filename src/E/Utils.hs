module E.Utils (module E.Utils, module X) where
import {-# source #-} E as X

e'bifoldMap :: (a -> r) -> (b -> r) -> E a b -> r
e'bifoldMap f g = \case
  L a -> f a
  R b -> g b

e'swap :: E a b -> E b a
e'swap = e'bifoldMap R L

e'bitraverse :: Map f => (a -> f x) -> (b -> f y) -> E a b -> f (E x y)
e'bitraverse f g = \case
  L a -> map L (f a)
  R b -> map R (g b)

e'bimap :: (x -> a) -> (y -> b) -> E x y -> E a b
e'bimap f g = e'bifoldMap (\a -> L (f a)) (\b -> R (g b))

e'map :: (y -> b) -> E a y -> E a b
e'map = e'bimap (\a -> a)

e'lmap :: (x -> a) -> E x b -> E a b
e'lmap = (`e'bimap` (\b -> b))
