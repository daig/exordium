module Sieve
  (Sieve(..)
  ,module X ) where
import Dimap as X
import Map as X
import Rep as X

class (Dimap p, Map (Rep p)) => Sieve p where
  sieve :: p a b -> a -> Rep p b

instance Sieve (->) where
  sieve f a = I (f a)
