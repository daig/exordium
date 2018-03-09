module Swap where
import Map.Di as X
import {-# source #-} E

-- | swap < swap = id
class Swap f where
  {-# minimal swap | swapped #-}
  swap :: f a b -> f b a
  swapped :: Dimap p => p (f b a) (f b' a') -> p (f a b) (f a' b')
  swapped = dimap swap swap
instance Swap (,) where swap (a,b) = (b,a)
instance Swap E where swap = \case {L a -> R a; R b -> L b}
