module Dipure (Dipure(..), module X) where
import Dimap as X
import Category as X

-- dimap f g (dipure h) = dipure (dimap f g h)
-- dipure id = id
class (Dimap p,Category p) => Dipure p where dipure :: (a -> b) -> p a b

instance Dipure (->) where dipure = id
