{-# language UndecidableInstances #-}
module Monad (module Monad, module X) where
import Bind as X
import Applicative as X
import Category as X
import qualified Prelude as P

-- | pure <=< f = f
--   f <=< pure = f
class (Bind m, Applicative m) => Monad m

instance Monad ((->) r)
instance Monad []
instance Monad m => P.Monad m where
  return = pure
  (>>=) = (X.>>=)

(>>) :: Monad m => m a -> m b -> m b
m >> m' = m >>= (\_ -> m')

g <=< f = \x -> g =<< f x
