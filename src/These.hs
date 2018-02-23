module These where
import Bimap.Class

data These a b = This a | That b | These a b

instance Bimap These where bimap = these'bimap
these'bimap f g = \case
  This a -> This (f a)
  That b -> That (g b)
  These a b -> These (f a) (g b)

instance MapL These where lmap = these'lmap
these'lmap = (`these'bimap` (\b -> b))

instance MapR These where rmap = these'map
these'map = these'bimap (\a -> a)
these'swap = \case
  This a -> That a
  That b -> This b
  These a b -> These b a
