module Tabulated (Tabulated(..), module X) where
import Rep.Type as X
import Traversed as X

class (Promap p, Map (Rep p)) => Tabulated p where
  tabulateP :: (a -> Rep p b) -> p a b
