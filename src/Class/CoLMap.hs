module Class.CoLMap where
import Type.K
import Flip

class CoLMap p where
  colmap :: (a -> x) -> p x b -> p a b
  {-default colmap :: forall a x b. (Comap (Flip p b)) => (a -> x) -> p x b -> p a b-}
  {-colmap = coerce# (comap @(Flip p b))-}

instance CoLMap (->) where colmap f p = \a -> p (f a)
instance CoLMap (Flipped K) where colmap _ (Flip (K b)) = Flip (K b)
