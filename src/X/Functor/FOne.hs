module X.Functor.FOne (FOne(..),module X) where
import X.Functor.Remap as X

class Remap f => FOne f where
  {-# minimal fone | pure #-}
  fone :: f ()
  fone = pure ()
  pure :: a -> f a
  pure a = remap (\_ -> ()) (\_ -> a) fone
