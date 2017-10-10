module Monad (Monad,module X) where
import Bind as X
import Pure as X

type Monad m = (Bind m, Pure m)
