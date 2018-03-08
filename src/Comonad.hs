module Comonad (Comonad, module X) where
import FoldMap as X
import Duplicate as X

-- | extend extract = id
-- extract < extend f = f
class (FoldMap_ w,Duplicate w) => Comonad w
