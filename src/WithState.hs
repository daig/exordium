module WithState (WithState(..), module X) where
import Monad as X

-- | get >> get = get
--   put s >> get = pure s
class Monad m => WithState m where
  {-# minimal withState | get,put #-}
  type State m :: *
  withState :: (State m -> (a,State m)) -> m a
  withState f = get >>= (\(f -> (a,s)) -> put s >> pure a)
  get :: m (State m)
  get = withState (\s -> (s,s))
  put :: State m -> m ()
  put s = withState (\_ -> ((),s))
