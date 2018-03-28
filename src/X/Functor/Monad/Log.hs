module X.Functor.Monad.Log (MonadLog(..), module X) where
import X.Functor.Monad as X

class Monad m => MonadLog m where
  {-# minimal (log | tell), pass, listen #-}
  type Log m :: *
  log :: (Log m,a) -> m a
  log ~(w,a) = a `constMap` tell w
  pass :: m (a, Log m -> Log m) -> m a
  tell :: Log m -> m ()
  tell w = log (w,())
  listen :: m a -> m (a,Log m)
