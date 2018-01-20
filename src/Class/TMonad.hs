module Class.TMonad (module Class.TMonad, module X) where
import Class.TMap as X
import Class.Monad as X

class MapT t => MonadT t where
  tbind :: Monad n => (m --> t n) -> (t m --> t n)
  tbind mtn tm = tjoin (tmap mtn tm)
  tjoin :: Monad m => t (t m) --> t m
  tjoin = tbind (\x -> x)


