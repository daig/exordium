{-# language UndecidableInstances #-}
module Monad.Class (module Monad.Class, module X) where
import Bind.Class as X
import Applicative.Class as X
import qualified Prelude as P

-- | pure <=< f = f
--   f <=< pure = f
class (Bind m, Applicative m) => Monad m

instance Monad ((->) r)
instance Monad []
