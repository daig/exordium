module TMonad.Class (module TMonad.Class, module X) where
import TMap.Class as X
import Monad.Class as X

class TMap t => MonadT t where
  tbind :: Monad n => (m --> t n) -> (t m --> t n)
  tbind mtn tm = tjoin (tmap mtn tm)
  tjoin :: Monad m => t (t m) --> t m
  tjoin = tbind (\x -> x)


