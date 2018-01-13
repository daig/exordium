module Tabulated (Tabulated(..), module X) where
import Rep as X
import Lens as X
import Dimap as X
import Map as X

class (Dimap p, Map (Rep p)) => Tabulated p where
  tabulateP :: (a -> Rep p b) -> p a b
