module Arrow.Sieve (Sieve(..), module X) where
import Arrow.Promap as X
import Rep.Type as X
import Type.I

class (Promap p, Map (Rep p)) => Sieve p where sieve :: p a b -> a -> Rep p b

instance Sieve (->) where sieve f a = I (f a)
