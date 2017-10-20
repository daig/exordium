module These where
import Bimap
import Swap

data These a b = This a | That b | These a b
instance Bimap These where
  bimap f g = \case
    This a -> This (f a)
    That b -> That (g b)
    These a b -> These (f a) (g b)
instance MapL These where mapL = lmap
instance MapR These where mapR = rmap
instance Swap These where
  swap = \case
    This a -> That a
    That b -> This b
    These a b -> These b a
