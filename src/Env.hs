module Env (Env(..), module X) where
import Monad.Class as X

-- | ask >> ask = ask
-- env (\_ -> a) = pure a
-- local f >=> local g = local (f > g)
-- local f >>= ask = 
class Monad m => Env m where
  {-# minimal (env | ask), local #-}
  type EnvOf m
  env :: (EnvOf m -> a) -> m a
  env f = map f ask
  ask :: m (EnvOf m)
  ask = env (\c -> c)

instance Env ((->) r) where
  type EnvOf ((->) r) = r
  env f = f
