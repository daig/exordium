module Compose where

-- f < (g < h) = (f < g) < h
-- if (p :: * -> * -> *) then instance Dipure p
class Compose p where
  {-# minimal (<) | (>) #-}
  (<) :: p x b -> p a x -> p a b
  p < q = q > p
  (>) :: p a x -> p x b -> p a b
  p > q = q < p

infixr 9 <,>

instance Compose (->) where
  f < g = \a -> f (g a)
  f > g = \a -> g (f a)
