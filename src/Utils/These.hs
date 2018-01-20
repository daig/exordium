module Utils.These (module Utils.These, module X) where
import Type.These as X

these'bimap f g = \case
  This a -> This (f a)
  That b -> That (g b)
  These a b -> These (f a) (g b)
these'lmap = (`these'bimap` (\b -> b))
these'map = these'bimap (\a -> a)
these'swap = \case
  This a -> That a
  That b -> This b
  These a b -> These b a
