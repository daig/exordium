module X.Arrow.Tabulated (Tabulated(..), module X) where
import X.Rep.Type as X
import X.Arrow.Traversed as X

class (Promap p, Map (Rep p)) => Tabulated p where
  tabulateP :: (a -> Rep p b) -> p a b
