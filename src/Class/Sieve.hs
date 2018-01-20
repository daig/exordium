module Class.Sieve (module Class.Sieve, module X) where
import Class.Dimap as X
import Type.Rep as X

class (Dimap p, Map (Rep p)) => Sieve p where sieve :: p a b -> a -> Rep p b

instance Sieve (->) where sieve f a = I (f a)
