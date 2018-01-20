module Class.LMap where
import Utils.Tuple
import Utils.K
import Utils.E
import Utils.These
import Utils.Where

class LMap p where
  lmap :: (x -> a) -> p x b -> p a b
  {-default lmap :: forall a x b. Map (Flip p b) => (x -> a) -> p x b -> p a b-}
  {-lmap = coerce# (map @(Flip p b))-}

instance LMap (,) where lmap = tuple'lmap
instance LMap K where lmap = k'lmap
instance LMap These where lmap = these'lmap
instance LMap Where where lmap = where'lmap
instance LMap E where lmap = e'lmap
