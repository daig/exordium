module X.Functor.Bipure (module X.Functor.Bipure, module X) where
import X.Functor.Bimap as X

class Bimap p => Bipure p where
  purel :: a -> p a b
  purer :: b -> p a b
-- bimap f _ (purel a) = purel (f a)
-- bimap _ g (purer b) = purer (g a)

