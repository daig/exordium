module Functor.Monad.Env (MonadEnv(..), module X) where
import Functor.Monad as X

-- | ask >> ask = ask
-- env (\_ -> a) = pure a
-- local f >=> local g = local (f > g)
-- local f >>= ask = 
class Monad m => MonadEnv m where
  {-# minimal env | ask #-}
  type Env m
  env :: (Env m -> a) -> m a
  env f = map f ask
  ask :: m (Env m)
  ask = env (\c -> c)

instance MonadEnv ((->) r) where
  type Env ((->) r) = r
  env f = f
