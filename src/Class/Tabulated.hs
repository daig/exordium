module Class.Tabulated (module Class.Tabulated, module X) where
import Type.Rep as X
import Class.Lens as X

class (Dimap p, Map (Rep p)) => Tabulated p where
  tabulateP :: (a -> Rep p b) -> p a b
