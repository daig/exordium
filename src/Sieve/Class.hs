module Sieve.Class (module Sieve.Class, module X) where
import Dimap.Class as X
import Rep.Type as X
import I
import Star.Type

class (Dimap p, Map (Rep p)) => Sieve p where sieve :: p a b -> a -> Rep p b

instance Sieve (->) where sieve f a = I (f a)
