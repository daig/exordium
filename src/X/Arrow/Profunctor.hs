module X.Arrow.Profunctor (Profunctor(..), module X) where
import X.Arrow.Compose as X

-- | profmap (f < g) = profmap f < profmap g
class (Compose q) => Profunctor q p where profmap :: q a b -> p b c -> p a c
