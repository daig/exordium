module StrongCompose (StrongCompose(..), module X) where
import Compose as X
import Strong as X

class (Strong p, Compose p) => StrongCompose p where
  (***) :: p a b -> p a' b' -> p (a,a') (b,b')
  f *** g = first f > second g
