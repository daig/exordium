module X.Functor.Swap where
import X.Arrow.Promap as X
import X.Data.E
import X.Data.These

-- | swap < swap = id
class Swap f where
  {-# minimal swap #-}
  swap :: f a b -> f b a
  swapped :: Promap p => p (f b a) (f b' a') -> p (f a b) (f a' b')
  swapped = promap swap swap
instance Swap (,) where swap (a,b) = (b,a)
instance Swap E where swap = \case {L a -> R a; R b -> L b}
instance Swap These where
  swap = \case
    This a -> That a
    That b -> This b
    These a b -> These b a
