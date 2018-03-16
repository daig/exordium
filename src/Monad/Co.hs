module Monad.Co (Comonad, module X) where
import Fold as X
import Duplicate as X

-- | extend extract = id
-- extract < extend f = f
class (Fold_ w,Duplicate w) => Comonad w

instance Comonad ((,) x)
