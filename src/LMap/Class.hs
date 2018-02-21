module LMap.Class where
import Tuple

class LMap p where
  lmap :: (x -> a) -> p x b -> p a b
  {-default lmap :: forall a x b. Map (Flip p b) => (x -> a) -> p x b -> p a b-}
  {-lmap = coerce# (map @(Flip p b))-}

instance LMap (,) where lmap = tuple'lmap
