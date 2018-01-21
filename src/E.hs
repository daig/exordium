module E (module E, module X) where
import E.Type as X

e'bifoldMap_ f g = \case
  L a -> f a
  R b -> g b
e'swap = e'bifoldMap_ R L
e'bimap f g = e'bifoldMap_ (\a -> L (f a)) (\b -> R (g b))
e'map = e'bimap (\a -> a)
e'lmap = (`e'bimap` (\b -> b))
