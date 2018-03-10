module Map.L where
import {-# source #-} E

class MapL p where
  lmap :: (x -> a) -> p x b -> p a b
  {-default lmap :: forall a x b. Map (Flip p b) => (x -> a) -> p x b -> p a b-}
  {-lmap = coerce# (map @(Flip p b))-}

instance MapL (,) where lmap f (x,y) = (f x, y)

instance MapL E where
  lmap f = \case
    L a -> L (f a)
    R b -> R b
