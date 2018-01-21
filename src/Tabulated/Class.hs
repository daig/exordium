module Tabulated.Class (module Tabulated.Class, module X) where
import Type.Rep as X
import Lens.Class as X

class (Dimap p, Map (Rep p)) => Tabulated p where
  tabulateP :: (a -> Rep p b) -> p a b
