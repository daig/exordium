module ADT.These where
import Functor.Bimap
import Functor.Swap as X

data These a b = This a | That b | These a b

instance Map (These a) where map = rmap
instance Bimap These where
  bimap f g = \case
    This a -> This (f a)
    That b -> That (g b)
    These a b -> These (f a) (g b)

instance Swap These where
  swap = \case
    This a -> That a
    That b -> This b
    These a b -> These b a
