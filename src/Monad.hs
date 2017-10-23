{-# language UndecidableInstances #-}
module Monad (Monad,module X) where
import Bind as X
import Applicative as X
import qualified Prelude as P

class (Bind m, Applicative m) => Monad m

instance Monad []
instance Monad m => P.Monad m where
  return = pure
  (>>=) = (X.>>=)
