module X.Arrow.Sieve (Sieve(..), module X) where
import X.Functor.Map as X
import X.Arrow.Promap as X
import X.Rep.Type as X
import X.Type.I

class (Promap p, Map (Rep p)) => Sieve p where sieve :: p a b -> a -> Rep p b

instance Sieve (->) where sieve f a = I (f a)
