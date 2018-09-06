module X.Arrow.Propure (Propure(..),module X) where
import X.Arrow.Category as X
import X.Arrow.Promap as X

class (Category p, Promap p) => Propure p where
  propure :: (a -> b) -> p a b
  propure f = premap f identity
