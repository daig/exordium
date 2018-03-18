module Arrow.Compose where

-- f < (g < h) = (f < g) < h
-- if (p :: * -> * -> *) then instance Arr p
class Compose p where
  {-# minimal precompose | postcompose #-}
  precompose :: p a x -> p x b -> p a b
  precompose p q = postcompose q p
  postcompose :: p x b -> p a x -> p a b
  postcompose p q = precompose q p
(<) :: Compose p => p x b -> p a x -> p a b
(<) = postcompose
(>) :: Compose p => p a x -> p x b -> p a b
(>) = precompose

infixr 9 <,>

instance Compose (->) where
  postcompose f g = \a -> f (g a)
  precompose f g = \a -> g (f a)
