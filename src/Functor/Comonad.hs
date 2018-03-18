module Functor.Comonad (Comonad, module X) where
import Functor.Fold as X
import Functor.Duplicate as X

-- | extend extract = id
-- extract < extend f = f
class (Fold_ w,Duplicate w) => Comonad w

instance Comonad ((,) x)
