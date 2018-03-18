module Functor.Swap where
import Arrow.Promap as X
import {-# source #-} ADT.E

-- | swap < swap = id
class Swap f where
  {-# minimal swap | swapped #-}
  swap :: f a b -> f b a
  swapped :: Promap p => p (f b a) (f b' a') -> p (f a b) (f a' b')
  swapped = promap swap swap
instance Swap (,) where swap (a,b) = (b,a)
instance Swap E where swap = \case {L a -> R a; R b -> L b}
