module Comap.L where

class ComapL p where
  colmap :: (a -> x) -> p x b -> p a b
  {-default colmap :: forall a x b. (Comap (Flip p b)) => (a -> x) -> p x b -> p a b-}
  {-colmap = coerce# (comap @(Flip p b))-}

instance ComapL (->) where colmap f p = \a -> p (f a)
