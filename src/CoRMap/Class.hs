module CoRMap.Class where
import K.Type

class CoRMap p where
  cormap :: (b -> x) -> p a x -> p a b
  {-default cormap :: forall a x b. (Comap (Flip p b)) => (a -> x) -> p x b -> p a b-}
  {-cormap = coerce# (comap @(Flip p b))-}

instance CoRMap K where cormap _ (K a) = K a
