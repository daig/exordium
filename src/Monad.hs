module Monad (Monad,module X) where
import Bind as X
import Applicative as X

-- | pure <=< f = f
--   f <=< pure = f
class (Bind m, Applicative m) => Monad m

instance Monad ((->) r)
instance Monad []

{-(>>) :: Monad m => m a -> m b -> m b-}
{-m >> m' = m >>= (\_ -> m')-}

{-g <=< f = \x -> g =<< f x-}
