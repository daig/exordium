{-# language UndecidableInstances #-}
module Class.Monad (module Class.Monad, module X) where
import Class.Bind as X
import Class.Applicative as X
import qualified Prelude as P

-- | pure <=< f = f
--   f <=< pure = f
class (Bind m, Applicative m) => Monad m

instance Monad ((->) r)
instance Monad []
