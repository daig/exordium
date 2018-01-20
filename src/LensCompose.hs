module LensCompose (LensCompose(..), module X) where
import Class.Compose as X
import Class.Lens as X

class (Lens p, Compose p) => LensCompose p where
  (***) :: p a b -> p a' b' -> p (a,a') (b,b')
  f *** g = first f > second g
