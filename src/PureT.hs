module PureT (PureT(..), module X) where
import MapT as X
import Monad as X
import Forall as X

class MapT t => PureT t where
  pureT :: Monad m => m --> t m
