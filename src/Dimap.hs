{-# language MagicHash #-}
module Dimap where
import Sum

class Dimap p where
  {-# minimal dimap | premap,postmap #-}
  dimap :: (a -> x) -> (y -> b) -> p x y -> p a b
  dimap f g h = postmap g (premap f h)
  premap :: (a -> x) -> p x b -> p a b
  premap f = dimap f (\x -> x)
  postmap :: (y -> b) -> p a y -> p a b
  postmap = dimap (\x -> x)

instance Dimap (->) where
  dimap f g h a = g (h (f a))

class Dimap p => Costrong p where
  unfirst :: p (a,x) (b,x) -> p a b
  unsecond :: p (x,a) (x,b) -> p a b

class Dimap p => Cochoice p where
  unleft :: p (E a x) (E b x) -> p a b
  unright :: p (E x a) (E x b) -> p a b
