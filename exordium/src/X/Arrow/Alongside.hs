module X.Arrow.Alongside (Alongside(..), module X) where
import X.Arrow.Promap as X

class Promap p => Alongside p where
  alongside :: p a b -> p a' b' -> p (a,a') (b,b')
