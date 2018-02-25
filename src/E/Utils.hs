module E.Utils (module E.Utils, module X) where
import {-# source #-} E as X

e'bifoldMap f g = \case
  L a -> f a
  R b -> g b

e'swap = e'bifoldMap R L

e'bitraverse f g = \case
  L a -> map L (f a)
  R b -> map R (g b)

e'bimap f g = e'bifoldMap (\a -> L (f a)) (\b -> R (g b))
e'map = e'bimap (\a -> a)
e'lmap = (`e'bimap` (\b -> b))
