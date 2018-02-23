module Tabulated.Class (module Tabulated.Class, module X) where
import Rep.Type as X
import Traversed_.Class as X

class (Dimap p, Map (Rep p)) => Tabulated p where
  tabulateP :: (a -> Rep p b) -> p a b
