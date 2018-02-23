module Map.L where
import Tuple

class MapL p where
  lmap :: (x -> a) -> p x b -> p a b
  {-default lmap :: forall a x b. Map (Flip p b) => (x -> a) -> p x b -> p a b-}
  {-lmap = coerce# (map @(Flip p b))-}

instance MapL (,) where lmap = tuple'lmap
