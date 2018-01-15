module IWithEnv (IWithEnv(..), module X) where

import WithEnv as X
import Forall as X
class ForallF WithEnv m => IWithEnv m where
  local' :: (Env (m j) -> Env (m i)) -> m i a -> m j a

instance IWithEnv (->) where
  local' f g r = g (f r)
