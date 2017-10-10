module Dipure (Dipure(..), module X) where
import Dimap as X
import Category as X

-- dimap f g (dipure h) = dipure (dimap f g h)
-- dipure id = id
class (Dimap p,Category p) => Dipure p where
  {-# minimal dipure #-}
  dipure :: (a -> b) -> p a b
  constP :: b -> p x b
  constP = \b -> dipure (\_ -> b)

instance Dipure (->) where
  dipure = id
  constP = \b _ -> b
