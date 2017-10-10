module Strong (Strong(..), ($:), (.&), (&.), module X) where
import Dimap as X
import Swap as X

class Dimap p => Strong p where
  {-# minimal first | second #-}
  first :: p a b -> p (a,y) (b,y)
  first = \p -> dimap swap swap (second p)
  second :: p a b -> p (x,a) (x,b)
  second = \p -> dimap swap swap (first p)

($:) :: Strong p => p a (b -> c) -> p (a,b) c
($:) = \p -> (\(f,x) -> f x) `postmap` first p

