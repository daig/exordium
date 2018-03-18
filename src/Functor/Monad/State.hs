module Functor.Monad.State (MonadState(..), module X) where
import Functor.Monad as X

-- | get >> get = get
--   put s >> get = pure s
class Monad m => MonadState m where
  {-# minimal state | get,put #-}
  type State m :: *
  state :: (State m -> (a,State m)) -> m a
  state f = bind (\(f -> (a,s)) -> a `constMap` put s) get
  get :: m (State m)
  get = state (\s -> (s,s))
  put :: State m -> m ()
  put s = state (\_ -> ((),s))
