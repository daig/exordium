module MonadT (MonadT(..), module X) where
import MapT as X
import Monad as X
import Forall as X

class MapT t => MonadT t where
  bindT :: Monad n => (m --> t n) -> (t m --> t n)
  bindT mtn tm = joinT (mapT mtn tm)
  joinT :: Monad m => t (t m) --> t m
  joinT = bindT (\x -> x)


