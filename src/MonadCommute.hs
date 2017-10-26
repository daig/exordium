module MonadCommute (MonadCommute(..), module X) where
import Monad as X
import Lens.Type as X (type (=~))

class (Monad m, Monad n) => MonadCommute m n where
  {-# minimal monadCommute | monadCommuteL, monadCommuteR #-}
  monadCommute :: (m (n a) =~ n (m a)) (n (m b)) (m (n b))
  monadCommuteL :: m (n a) -> n (m a)
  monadCommuteR :: n (m a) -> m (n a)

