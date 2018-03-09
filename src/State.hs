module State (State(..), module X) where
import Monad as X

-- | get >> get = get
--   put s >> get = pure s
class Monad m => State m where
  {-# minimal state | get,put #-}
  type StateOf m :: *
  state :: (StateOf m -> (a,StateOf m)) -> m a
  state f = bind (\(f -> (a,s)) -> a `constMap` put s) get
  get :: m (StateOf m)
  get = state (\s -> (s,s))
  put :: StateOf m -> m ()
  put s = state (\_ -> ((),s))