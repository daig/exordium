module Unstrong (Unstrong(..), module X) where
import Dimap as X
import Swap as X

class Dimap p => Unstrong p where
  unfirst :: p (a,y) (b,y) -> p a b
  unfirst = \p -> unsecond (dimap swap swap p)
  unsecond :: p (x,a) (x,b) -> p a b
  unsecond = \p -> unfirst (dimap swap swap p)

instance Unstrong (->) where
  unfirst f = \a -> let (b,y) = f (a,y) in b
  unsecond f = \a -> let (x,b) = f (x,a) in b
