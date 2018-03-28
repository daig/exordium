module X.Mono.Map (Monomap(..), module X) where
import X.Arrow.Mapped as X

class Monomap t s b a | s -> a, t -> b, s b -> t, t a -> s where
  _map :: Mapped p => p a b -> p s t
  default _map :: (s ~ f a, t ~ f b, Map f,Mapped p) => p a b -> p s t
  _map = mapped

instance Monomap [b] [a] b a
