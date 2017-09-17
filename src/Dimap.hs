{-# language MagicHash #-}
module Dimap where
import Sum
import Coerce

class Dimap p where
  {-# minimal dimap | premap,postmap #-}
  dimap :: (a -> x) -> (y -> b) -> p x y -> p a b
  dimap f g h = postmap g (premap f h)
  premap :: (a -> x) -> p x b -> p a b
  premap f = dimap f (\x -> x)
  postmap :: (y -> b) -> p a y -> p a b
  postmap = dimap (\x -> x)
  premap# :: forall b a x. x =# b => p a x -> p a b
  premap# = coerce#

instance Dimap (->) where
  dimap f g h a = g (h (f a))

class Dimap p => Choice p where
  left :: p a b -> p (E a x) (E b x)
  right :: p a b -> p (E x a) (E x b)

class Dimap p => Strong p where
  first :: p a b -> p (a,x) (b,x)
  second :: p a b -> p (x,a) (x,b)

class Dimap p => Costrong p where
  unfirst :: p (a,x) (b,x) -> p a b
  unsecond :: p (x,a) (x,b) -> p a b

class Dimap p => Cochoice p where
  unleft :: p (E a x) (E b x) -> p a b
  unright :: p (E x a) (E x b) -> p a b
