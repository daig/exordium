module X.Functor.Monad.State (MonadState(..), module X) where
import X.Functor.Monad as X
import X.Kind.Type

-- | get >> get = get
--   put s >> get = pure s
class Monad m => MonadState m where
  {-# minimal state | get,put #-}
  type State m :: Type
  state :: (State m -> (a,State m)) -> m a
  state f = bind (\(f -> (a,s)) -> a `constMap` put s) get
  get :: m (State m)
  get = state (\s -> (s,s))
  put :: State m -> m ()
  put s = state (\_ -> ((),s))
