module Cochoice (Cochoice(..), module X) where
import Dimap as X
import Sum as X (E)
import Sum

class Dimap p => Cochoice p where
  {-# minimal unleft | unright #-}
  unleft :: p (E a x) (E b x) -> p a b
  unleft = \p -> unright (dimap swap swap p)
  unright :: p (E x a) (E x b) -> p a b
  unright = \p -> unleft (dimap swap swap p)


instance Cochoice (->) where
  unleft f = \a -> go (L a) where go = \a -> biextract (\x -> x) (\x -> go (R x)) (f a)
  unright f = \a -> go (R a) where go = \a -> biextract (\x -> go (L x)) (\x -> x) (f a)
