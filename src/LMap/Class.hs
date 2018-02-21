module LMap.Class where
import Tuple
import E

class LMap p where
  lmap :: (x -> a) -> p x b -> p a b
  {-default lmap :: forall a x b. Map (Flip p b) => (x -> a) -> p x b -> p a b-}
  {-lmap = coerce# (map @(Flip p b))-}

instance LMap (,) where lmap = tuple'lmap
instance LMap E where lmap = e'lmap
