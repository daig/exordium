module Map.L where

class MapL p where
  lmap :: (x -> a) -> p x b -> p a b
  {-default lmap :: forall a x b. Map (Flip p b) => (x -> a) -> p x b -> p a b-}
  {-lmap = coerce# (map @(Flip p b))-}

instance MapL (,) where lmap f (x,y) = (f x, y)
