module Monad (Monad,module X) where
import Bind as X
import Pure as X

class (Bind m, Pure m) => Monad m

instance Monad []
