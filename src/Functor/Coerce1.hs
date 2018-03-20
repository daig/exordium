module Functor.Coerce1 (Coerce1(..), module X) where
import Functor.Traverse as X
import Functor.Comap as X
import Type.K
import ADT.X

class (Traverse0 f, Comap f) =>  Coerce1 f where
  coerce1 :: f a -> f b
  coerce1 fa = map (\case) (comap @f @X (\case) fa)

instance Coerce1 (K a) where coerce1 (K a) = K a


coerce1_sequence0 :: (Pure f, Coerce1 t) =>  t (f a) -> f (t a)
coerce1_sequence0 t = pure (coerce1 t)
