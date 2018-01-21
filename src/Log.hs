module Log (Log(..), module X) where
import Monad.Class as X

class Monad m => Log m where
  {-# minimal (log | tell), pass, listen #-}
  type LogOf m :: *
  log :: (LogOf m,a) -> m a
  log ~(w,a) = a `constMap` tell w
  pass :: m (a, LogOf m -> LogOf m) -> m a
  tell :: LogOf m -> m ()
  tell w = log (w,())
  listen :: m a -> m (a,LogOf m)
