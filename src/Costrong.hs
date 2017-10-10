module Costrong (Costrong(..), module X) where
import Dimap as X
import Swap

class Dimap p => Costrong p where
  {-# minimal unfirst | unsecond #-}
  unfirst :: p (a,x) (b,x) -> p a b
  unfirst = \p -> unsecond (dimap swap swap p)
  unsecond :: p (x,a) (x,b) -> p a b
  unsecond = \p -> unfirst (dimap swap swap p)

instance Costrong (->) where
  unfirst f a = b where (b,x) = f (a,x)
  unsecond f a = b where (x,b) = f (x,a)
