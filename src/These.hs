module These where
import Bimap.Class
import Swap as X

data These a b = This a | That b | These a b

instance Bimap These where
  bimap f g = \case
    This a -> This (f a)
    That b -> That (g b)
    These a b -> These (f a) (g b)

instance MapL These where lmap = (`bimap` (\b -> b))
instance MapR These where rmap = map
instance Map (These a) where map = bimap (\a -> a)
instance MapIso (These a) where mapIso _ = map
instance Swap These where
  swap = \case
    This a -> That a
    That b -> This b
    These a b -> These b a
