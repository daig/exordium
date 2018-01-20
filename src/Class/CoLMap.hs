module Class.CoLMap where

class CoLMap p where
  colmap :: (a -> x) -> p x b -> p a b
  {-default colmap :: forall a x b. (Comap (Flip p b)) => (a -> x) -> p x b -> p a b-}
  {-colmap = coerce# (comap @(Flip p b))-}

instance CoLMap (->) where colmap f p = \a -> p (f a)
