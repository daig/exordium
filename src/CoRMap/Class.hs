module CoRMap.Class where

class CoRMap p where
  cormap :: (b -> x) -> p a x -> p a b
  {-default cormap :: forall a x b. (Comap (Flip p b)) => (a -> x) -> p x b -> p a b-}
  {-cormap = coerce# (comap @(Flip p b))-}

