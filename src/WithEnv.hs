module WithEnv (WithEnv(..), module X) where

import Monad as X

-- | ask >> ask = ask
-- withEnv (\_ -> a) = pure a
-- local f >=> local g = local (f > g)
-- local f >>= ask = 
class Monad m => WithEnv m where
  {-# minimal (withEnv | ask), local #-}
  type Env m
  withEnv :: (Env m -> a) -> m a
  withEnv f = map f ask
  ask :: m (Env m)
  ask = withEnv (\c -> c)
  local :: (Env m -> Env m) -> m a -> m a

instance WithEnv ((->) r) where
  type Env ((->) r) = r
  withEnv f = f
  local f g r = g (f r)
